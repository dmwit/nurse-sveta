module Nurse.Sveta.Tomcats (
	SearchContext(..), HyperParameters(..),
	RNGTree(..), MoveTree(..),
	newHyperParameters, newRNGTree, newRNGTreeFromSeed, newMoveTree,
	descendRNGTree, descendMoveTree,
	expandRNGTree', expandRNGTree, expandMoveTree', expandMoveTree,
	sampleRNG, bestMove, weightedMove, uniformMove, sampleMove,
	hpDiscount, hpImmediateReward, hpFinalReward, ihpFinalReward,
	ctxDiscount, ctxImmediateReward, ctxFinalReward,
	ctxIterations, ctxMaxLevel,
	ctxDiscountRate, ctxC_puct, ctxDirichlet, ctxPriorNoise,
	ctxRewardVirusClear, ctxRewardOtherClear, ctxRewardWin, ctxRewardLoss,
	playRNG, playMove,
	GameStateSeed(..),
	dumbEvaluation,
	MidPath(..),
	GameState(..), IGameState(..), cloneGameState, freezeGameState,
	ppRNGTree, ppRNGTreeDebug, ppMoveTree, ppMoveTreeDebug,
	ppMoveTrees, ppPlacements, ppEndpointMap, ppUnexploredMove,
	ppAeson,
	ppPill, ppContent, ppOrientation, ppColor,
	ppPosition,
	ppPercent, ppPrecision,
	) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Bits
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.List
import Data.Semigroup
import Data.Vector (Vector)
import Dr.Mario.Model
import Dr.Mario.Pathfinding
import Numeric
import Nurse.Sveta.STM.BatchProcessor
import Nurse.Sveta.Torch.Semantics
import System.Random.MWC
import System.Random.MWC.Distributions
import System.Random.Stateful (uniformFloat01M)

import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Nurse.Sveta.Torch.EndpointMap as EM
import qualified Data.Text as T

-- TODO:
-- 	* board/pill generation options like MWC vs LFSR, Algorithm X vs NES' algo,
-- 	  128 pre-determined pills vs on-the-fly, maybe SNES vs NES distro?
-- 	* neural net vs. not

data GameState = GameState
	{ board :: IOBoard
	, virusesKilled, pillsUsed, framesPassed :: IORef Int
	, lookbehind :: IORef Lookahead
	, originalVirusCount :: Int
	, originalSensitive :: Bool
	, speed :: CoarseSpeed
	}

uninitializedLookbehind :: IO (IORef Lookahead)
uninitializedLookbehind = newIORef (error "asked what pill was last launched before any pill was launched")

data IGameState = IGameState
	{ iBoard :: Board
	, iVirusesKilled, iPillsUsed, iFramesPassed :: Int
	, iLookbehind :: Lookahead
	, iOriginalVirusCount :: Int
	, iSensitive :: Bool
	, iSpeed :: CoarseSpeed
	} deriving (Eq, Ord, Read, Show)

freezeGameState :: GameState -> IO IGameState
freezeGameState gs = do
	b <- mfreeze (board gs)
	vk <- readIORef (virusesKilled gs)
	pu <- readIORef (pillsUsed gs)
	fp <- readIORef (framesPassed gs)
	lk <- readIORef (lookbehind gs)
	pure IGameState
		{ iBoard = b
		, iVirusesKilled = vk
		, iPillsUsed = pu
		, iFramesPassed = fp
		, iLookbehind = lk
		, iOriginalVirusCount = originalVirusCount gs
		, iSensitive = originalSensitive gs /= (fp .&. 1 == 1)
		, iSpeed = speed gs
		}

playRNG :: GameState -> Lookahead -> IO ()
playRNG = writeIORef . lookbehind

playMove :: GameState -> MidPath -> Pill -> IO (Maybe ClearResults)
playMove gs path pill = do
	mres <- mplaceDetails (board gs) pill
	mres <$ for_ mres \results -> do
		let counts = summarizeClearResults results
		-- defensive programming: these should naturally get forced during
		-- position evaluation, but why take the risk? let's just keep the
		-- thunk depth low
		modifyIORef' (virusesKilled gs) (clears counts +)
		modifyIORef' (pillsUsed gs) succ
		modifyIORef' (framesPassed gs) (approximateCostModel path pill counts +)

class GameStateSeed a where initialState :: a -> IO GameState

instance Gen a ~ GenIO => GameStateSeed (Gen a) where
	initialState g = initialState (g, 20)

instance (a ~ GenIO, b ~ Int) => GameStateSeed (a, b) where
	-- note to self: when the NES does it, it generates the pill sequence first,
	-- then the board
	initialState (g, maxLevel_) = do
		level <- uniformRM (0, maxLevel) g
		seed <- uniformRM (2, maxBound) g
		b <- mrandomBoard seed level
		frameParity <- uniformM g
		lb <- uninitializedLookbehind
		coarseSpeed <- ([Med, Hi, Ult] !!) <$> uniformRM (0,2) g
		vk <- newIORef 0
		pu <- newIORef 0
		fp <- newIORef 0
		pure GameState
			{ board = b
			, virusesKilled = vk
			, pillsUsed = pu
			, framesPassed = fp
			, lookbehind = lb
			, originalVirusCount = 4*(level + 1)
			, originalSensitive = frameParity
			, speed = coarseSpeed
			}
		where maxLevel = min 20 . max 0 $ maxLevel_

instance (a ~ Board, b ~ Bool, c ~ CoarseSpeed) => GameStateSeed (a, b, c) where
	initialState (b, sensitive, speed) = pure GameState
		<*> thaw b
		<*> newIORef 0
		<*> newIORef 0
		<*> newIORef 0
		<*> uninitializedLookbehind
		<*> pure (countViruses b)
		<*> pure sensitive
		<*> pure speed

instance GameStateSeed GameState where initialState = pure

data RNGTree = RNGTree
	{ childrenRNG :: HashMap Lookahead MoveTree
	, orderRNG :: Vector Lookahead
	, nextLookaheadRNG :: Int
	, placementsRNG :: HashMap MidPlacement MidPath
	, symmetricPlacementsRNG :: HashMap MidPlacement MidPath
	, childPriorsRNG :: EndpointMap Pill CFloat
	, priorProbabilityRNG :: Float
	, cumulativeValuationRNG :: Float
	, visitCountRNG :: Float
	} deriving (Eq, Ord, Read, Show)

data MoveTree = MoveTree
	{ childrenMove :: HashMap Pill RNGTree
	, unexploredMove :: Vector (Pill, Float) -- ^ prior probability, sorted descending
	, pathsMove :: HashMap Pill MidPath
	, visitCountMove :: Float
	} deriving (Eq, Ord, Read, Show)

data SearchContext = SearchContext
	{ ctxEval :: Procedure NetInput NetOutput
	, ctxRNG :: GenIO
	, ctxState :: GameState
	, ctxParams :: HyperParameters
	}

data HyperParameters = HyperParameters
	{ hpDiscountRate, hpC_puct, hpDirichlet, hpPriorNoise, hpMoveNoise
	, hpRewardVirusClear, hpRewardOtherClear, hpRewardWin, hpRewardLoss
		:: Float
	, hpIterations, hpMaxLevel :: Int
	} deriving (Eq, Ord, Read, Show)

instance ToJSON HyperParameters where
	toJSON hp = toJSON
		$  traverse (fromIntegral.) [hpIterations, hpMaxLevel] hp
		++ sequence [hpDiscountRate, hpC_puct, hpDirichlet, hpPriorNoise, hpMoveNoise, hpRewardVirusClear, hpRewardOtherClear, hpRewardWin, hpRewardLoss] hp
	toEncoding hp = toEncoding
		$  traverse (fromIntegral.) [hpIterations, hpMaxLevel] hp
		++ sequence [hpDiscountRate, hpC_puct, hpDirichlet, hpPriorNoise, hpMoveNoise, hpRewardVirusClear, hpRewardOtherClear, hpRewardWin, hpRewardLoss] hp

instance FromJSON HyperParameters where
	parseJSON v = parseJSON v >>= \case
		[i, mv, dr, c, d, pn, mn, rvc, roc, rw, rl] -> pure HyperParameters
			{ hpIterations = round i
			, hpMaxLevel = round mv
			, hpDiscountRate = dr
			, hpC_puct = c
			, hpDirichlet = d
			, hpPriorNoise = pn
			, hpMoveNoise = mn
			, hpRewardVirusClear = rvc
			, hpRewardOtherClear = roc
			, hpRewardWin = rw
			, hpRewardLoss = rl
			}
		_ -> typeMismatch "HyperParameters" v

-- | Some maybe-not-completely-insane defaults.
newHyperParameters :: HyperParameters
newHyperParameters = HyperParameters
	{ hpDiscountRate = 0.9998 -- yields a discount of about 0.1 after 3 minutes
	, hpC_puct = 0.5
	, hpDirichlet = 0.25 -- 10/(typical number of moves available=40)
	, hpPriorNoise = 0.1
	, hpMoveNoise = 0.2
	, hpRewardVirusClear = 0.1
	, hpRewardOtherClear = 0.01
	, hpRewardWin = 1
	, hpRewardLoss = -1
	, hpIterations = 200
	, hpMaxLevel = 0
	}

hpDiscount :: HyperParameters -> Int -> Float -> Float
hpDiscount hp n v = v * hpDiscountRate hp ^ n

hpRewardClear :: HyperParameters -> Shape -> Float
hpRewardClear hp = \case
	Virus -> hpRewardVirusClear hp
	_ -> hpRewardOtherClear hp

hpImmediateReward :: HyperParameters -> ClearResults -> Float
hpImmediateReward hp = goClear where
	goClear = \case
		NoClear -> 0
		Clear cs dRes -> getSum (foldMap rewardClear cs) + discount clearCost (goDrop dRes)
	goDrop = \case
		NoDrop -> 0
		Drop ds cRes -> discount (getMax (foldMap (Max . fst) ds) * dropCost) (goClear cRes)
	rewardClear = Sum . hpRewardClear hp . oshape
	discount = hpDiscount hp

-- | does not modify the GameState
hpFinalReward :: HyperParameters -> GameState -> IO Float
hpFinalReward hp gs = do
	clearedViruses <- readIORef (virusesKilled gs)
	pure if clearedViruses == originalVirusCount gs
		then hpRewardWin hp
		else hpRewardLoss hp

ihpFinalReward :: HyperParameters -> IGameState -> Float
ihpFinalReward hp igs = if iVirusesKilled igs == iOriginalVirusCount igs
	then hpRewardWin hp
	else hpRewardLoss hp

ctxDiscountRate, ctxC_puct, ctxDirichlet, ctxPriorNoise, ctxMoveNoise, ctxRewardVirusClear, ctxRewardOtherClear, ctxRewardWin, ctxRewardLoss :: SearchContext -> Float
[ctxDiscountRate, ctxC_puct, ctxDirichlet, ctxPriorNoise, ctxMoveNoise, ctxRewardVirusClear, ctxRewardOtherClear, ctxRewardWin, ctxRewardLoss] = map (. ctxParams)
	[hpDiscountRate, hpC_puct, hpDirichlet, hpPriorNoise, hpMoveNoise, hpRewardVirusClear, hpRewardOtherClear, hpRewardWin, hpRewardLoss]

ctxIterations, ctxMaxLevel :: SearchContext -> Int
[ctxIterations, ctxMaxLevel] = map (.ctxParams) [hpIterations, hpMaxLevel]

ctxDiscount :: SearchContext -> Int -> Float -> Float
ctxDiscount = hpDiscount . ctxParams

newRNGTree :: SearchContext -> Float -> IO RNGTree
newRNGTree ctx prior = snd <$> newRNGTreeFromSeed (ctxEval ctx) (ctxRNG ctx) prior (ctxState ctx)

newRNGTree' :: SearchContext -> Float -> IO (Float, RNGTree)
newRNGTree' ctx prior = (\t -> (cumulativeValuationRNG t, t)) <$> newRNGTree ctx prior

-- TODO: could return an IO (GameState, IO RNGTree) to let the caller choose when to wait on the net evaluation
newRNGTreeFromSeed :: GameStateSeed a => Procedure NetInput NetOutput -> GenIO -> Float -> a -> IO (GameState, RNGTree)
newRNGTreeFromSeed eval rng prior seed = do
	gs <- initialState seed
	future <- schedule eval =<< netInput gs
	visitOrder <- uniformShuffle allLookaheads rng

	fp <- readIORef (framesPassed gs)
	pu <- readIORef (pillsUsed gs)
	placements <- mapproxReachable (board gs) (fp .&. 1 /= fromEnum (originalSensitive gs)) (gravity (speed gs) pu)

	no <- future

	let t = RNGTree
	    	{ childrenRNG = HM.empty
	    	, orderRNG = visitOrder
	    	, nextLookaheadRNG = 0
	    	, placementsRNG = placements
	    	, symmetricPlacementsRNG = symmetrize placements
	    	, childPriorsRNG = noPriors no
	    	, priorProbabilityRNG = prior
	    	, cumulativeValuationRNG = noValuation no
	    	, visitCountRNG = 1
	    	}

	pure (gs, t)
	where
	symmetrize :: HashMap MidPlacement MidPath -> HashMap MidPlacement MidPath
	symmetrize moves = HM.fromListWith shorterPath [(placement { mpRotations = mpRotations placement .&. 1 }, path) | (placement, path) <- HM.toList moves]

newMoveTree :: SearchContext -> HashMap MidPlacement MidPath -> EndpointMap Pill CFloat -> Lookahead -> IO MoveTree
newMoveTree ctx pathCache priors lk = do
	done <- finished (ctxState ctx)
	noise <- if done || ctxPriorNoise ctx == 0
		then pure (0 <$ pills)
		else dirichlet (realToFrac (ctxDirichlet ctx) <$ pills) (ctxRNG ctx)
	pure MoveTree
		{ childrenMove = HM.empty
		, unexploredMove = if done then V.empty else unexplored noise
		, pathsMove = if done then HM.empty else paths
		, visitCountMove = 0
		}
	where
	pills = V.fromList (HM.keys paths)
	unexplored = id
		. V.modify (V.sortBy (\(pill, prior) (pill', prior') -> compare prior' prior <> compare pill pill'))
		. V.zipWith (\pill noise -> (pill, lerp (ctxPriorNoise ctx) (realToFrac noise) ((priors EM.! pill) / normalizationFactor))) pills
	paths = HM.fromListWithKey (\pill -> error $ "Two different MidPlacements both got specialized to " ++ show pill ++ ". Original path cache: " ++ show pathCache)
		[ (mpPill placement lk, path)
		| (placement, path) <- HM.toList pathCache
		]
	Sum normalizationFactor = unzero <$> HM.foldMapWithKey (\pill _ -> Sum (priors EM.! pill)) paths

-- | modifies the GameState
descendRNGTree :: SearchContext -> RNGTree -> Lookahead -> IO MoveTree
descendRNGTree ctx t lk = do
	playRNG (ctxState ctx) lk
	case HM.lookup lk (childrenRNG t) of
		Nothing -> newMoveTree ctx (placements t) (childPriorsRNG t) lk
		Just t' -> pure t'
	where
	placements | leftColor lk == rightColor lk = symmetricPlacementsRNG
	           | otherwise = placementsRNG

-- | modifies the GameState
descendMoveTree :: SearchContext -> MoveTree -> Pill -> IO RNGTree
descendMoveTree ctx t pill = case HM.lookup pill (pathsMove t) of
	Nothing -> die True False "Illegal play (no path)"
	Just path -> playMove (ctxState ctx) path pill >>= \case
		Nothing -> die False False "Illegal play (occupied or out-of-bounds placement)"
		Just _ -> case HM.lookup pill (childrenMove t) of
			Just t' -> pure t'
			Nothing -> case V.find (\(pill', _) -> pill == pill') (unexploredMove t) of
				Nothing -> die True True "Tried to play a move with no known prior"
				Just (_, prior) -> newRNGTree ctx prior
	where
	die verbose played msg = do
		b <- mfreeze (board (ctxState ctx))
		fail $ msg
			++ "; attempted pill placement was " ++ ppPill pill
			++ " leading " ++ (if played then "to" else "from") ++ " this board:\n" ++ pp b
			++ "current search tree:\n" ++ (if verbose then ppMoveTreeDebug else ppMoveTree) "  " t

-- | modifies the GameState
expandRNGTree' :: SearchContext -> RNGTree -> IO (Float, RNGTree)
expandRNGTree' ctx t = do
	child <- descendRNGTree ctx t lk
	(v, child') <- expandMoveTree' ctx child
	let t' = t
	    	{ childrenRNG = HM.insert lk child' (childrenRNG t)
	    	, nextLookaheadRNG = lk'
	    	, placementsRNG = plc'
	    	, symmetricPlacementsRNG = symPlc'
	    	, childPriorsRNG = childP'
	    	, cumulativeValuationRNG = v + cumulativeValuationRNG t
	    	, visitCountRNG = 1 + visitCountRNG t
	    	}
	pure (v, t')
	where
	lk = orderRNG t V.! nextLookaheadRNG t
	lk' = (nextLookaheadRNG t + 1) `mod` V.length (orderRNG t)
	(childP', plc', symPlc') = if lk' < nextLookaheadRNG t
		then (uniformPriors, HM.empty, HM.empty)
		else (childPriorsRNG t, placementsRNG t, symmetricPlacementsRNG t)

-- | modifies the GameState
expandMoveTree' :: SearchContext -> MoveTree -> IO (Float, MoveTree)
expandMoveTree' ctx t = case (maximumOn (\_ -> scoreTree) (childrenMove t), scorePrior <$> unexploredMove t V.!? 0) of
	(Just (pill, t', score1), Just score2)
		| score1 < score2 -> explore
		| otherwise -> exploit pill t'
	(Just (pill, t', _), Nothing) -> exploit pill t'
	(Nothing, Just _) -> explore
	(Nothing, Nothing) -> flip (,) t { visitCountMove = visitCountMove t + 1 } <$> ctxFinalReward ctx
	where
	scoreTree t' = pucb (ctxC_puct ctx) (priorProbabilityRNG t') (visitCountMove t) (visitCountRNG t') (cumulativeValuationRNG t')
	scorePrior (_, prior) = pucb (ctxC_puct ctx) prior (visitCountMove t) 0 0

	explore = let (pill, prior) = V.head (unexploredMove t) in go pill (newRNGTree' ctx prior) t { unexploredMove = V.tail (unexploredMove t) }
	exploit pill t' = go pill (expandRNGTree' ctx t') t
	go pill buildTree t' = do
		before <- readIORef (framesPassed (ctxState ctx))
		Just results <- playMove (ctxState ctx) (pathsMove t HM.! pill) pill
		after <- readIORef (framesPassed (ctxState ctx))
		(dv, tNew) <- buildTree
		pure ( ctxImmediateReward ctx results + ctxDiscount ctx (after - before) dv
		     , t' { childrenMove = HM.insert pill tNew (childrenMove t'), visitCountMove = visitCountMove t' + 1 }
		     )

withClonedState :: (SearchContext -> a -> IO b) -> SearchContext -> a -> IO b
withClonedState f ctx a = do
	gs <- cloneGameState (ctxState ctx)
	f ctx { ctxState = gs } a

-- | does not modify the GameState
expandRNGTree :: SearchContext -> RNGTree -> IO RNGTree
expandRNGTree ctx t = snd <$> withClonedState expandRNGTree' ctx t

-- | does not modify the GameState
expandMoveTree :: SearchContext -> MoveTree -> IO MoveTree
expandMoveTree ctx t = snd <$> withClonedState expandMoveTree' ctx t

bestMove, weightedMove, uniformMove, sampleMove :: SearchContext -> MoveTree -> IO (Maybe Pill)
bestMove ctx = ensureNonEmpty \t -> case bestMoves' (childrenMove t) of
	BestMoves' pills _ | not (V.null pills) -> uniformV ctx pills
	_ -> fst <$> uniformV ctx (unexploredMove t)

weightedMove ctx = ensureNonEmpty \t -> let
	(childMoves, childTs) = V.unzip  . V.fromList . HM.toList $ childrenMove t
	moves = childMoves <> (fst <$> unexploredMove t)
	weights = (realToFrac . (1+) . visitCountRNG <$> childTs) <> (1 <$ unexploredMove t)
	in (moves V.!) <$> categorical weights (ctxRNG ctx)

uniformMove ctx = ensureNonEmpty \t -> uniformV ctx
	$  V.fromList (HM.keys (childrenMove t))
	<> fmap fst (unexploredMove t)

-- With probability 1-p, choose the move with the most visits. If that doesn't
-- happen, with probability 1-p, choose using the visit counts as weights for a
-- (lightly smoothed) categorical distribution. Otherwise choose completely
-- uniformly at random.
sampleMove ctx t = do
	v <- uniformFloat01M (ctxRNG ctx)
	case (v < p^2, v < p) of
		(True, _) -> uniformMove ctx t
		(_, True) -> weightedMove ctx t
		_ -> bestMove ctx t
	where
	p = ctxMoveNoise ctx

sampleRNG :: SearchContext -> IO Lookahead
sampleRNG ctx = uniformV ctx allLookaheads

ensureNonEmpty :: (MoveTree -> IO a) -> MoveTree -> IO (Maybe a)
ensureNonEmpty f t = if HM.null (childrenMove t) && V.null (unexploredMove t)
	then pure Nothing
	else Just <$> f t

-- | Caller must ensure the vector is nonempty
uniformV :: SearchContext -> Vector a -> IO a
uniformV ctx v = (v V.!) <$> uniformR (0, V.length v - 1) (ctxRNG ctx)

data BestMoves' = BestMoves' (Vector Pill) Float deriving (Eq, Ord, Read, Show)

instance Semigroup BestMoves' where
	bm@(BestMoves' mvs n) <> bm'@(BestMoves' mvs' n') = case compare n n' of
		LT -> bm'
		EQ -> BestMoves' (mvs <> mvs') n
		GT -> bm

instance Monoid BestMoves' where mempty = BestMoves' V.empty (-1/0)

bestMoves' :: HashMap Pill RNGTree -> BestMoves'
bestMoves' = HM.foldMapWithKey \pill t -> BestMoves' (V.singleton pill) (visitCountRNG t)

ctxImmediateReward :: SearchContext -> ClearResults -> Float
ctxImmediateReward = hpImmediateReward . ctxParams

-- | does not modify the GameState
ctxFinalReward :: SearchContext -> IO Float
ctxFinalReward ctx = hpFinalReward (ctxParams ctx) (ctxState ctx)

allLookaheads :: Vector Lookahead
-- could use EndpointKey to not have constants 9 and 3 here, but the result is completely unreadable
allLookaheads = V.generate (product (indexCounts' @Lookahead)) fromIndex

uniformPriors :: EndpointMap Pill CFloat
uniformPriors = EM.EndpointMap (foldr (svReplicate . evalGameConstant) (generate [] \_ -> 1) (indexCounts @Pill))

finished :: GameState -> IO Bool
finished gs = do
	cellL <- mget (board gs) startingBottomLeftPosition
	cellR <- mget (board gs) startingOtherPosition
	remaining <- (originalVirusCount gs-) <$> readIORef (virusesKilled gs)
	pure $ cellL /= Just Empty || cellR /= Just Empty || remaining <= 0

cloneGameState :: GameState -> IO GameState
cloneGameState gs = pure GameState
	<*> cloneBoard (board gs)
	<*> cloneIORef (virusesKilled gs)
	<*> cloneIORef (pillsUsed gs)
	<*> cloneIORef (framesPassed gs)
	<*> cloneIORef (lookbehind gs)
	<*> pure (originalVirusCount gs)
	<*> pure (originalSensitive gs)
	<*> pure (speed gs)

cloneIORef :: IORef a -> IO (IORef a)
cloneIORef = readIORef >=> newIORef

cloneBoard :: IOBoard -> IO (IOBoard)
cloneBoard = mfreeze >=> thaw

yCosts :: V.Vector Int
yCosts = V.fromList [47 {- TODO -}, 46 {- TODO -}, 44, 42, 41, 41, 39, 39, 37, 37, 35, 35, 33, 34, 34, 34]

dropCost :: Int
dropCost = 16 -- how long it takes for trash to drop one row

clearCost :: Int
clearCost = 20 -- how long the clear animation takes

-- TODO: figure out the exact cost model for the NES, then move that into maryodel
-- TODO: don't count fall time after the last clear
approximateCostModel :: MidPath -> Pill -> CleanupResults -> Int
approximateCostModel move pill counts = 0
	+ yCosts V.! y (bottomLeftPosition pill) -- throwing animation, lock animation, and pause between lock and next throw
	+ mpPathLength move -- pill maneuvering
	+ sum [dropCost*n + clearCost | n <- rowsFallen counts] -- fall time + clear animation

-- TODO: perhaps one day we should think about how to fuse this with toEndpoint
netInput :: GameState -> IO NetInput
netInput s = pure NetInput
	<*> mfreeze (board s)
	<*> readIORef (framesPassed s)
	<*> pure (originalVirusCount s)

dumbEvaluation :: NetInput -> IO NetOutput
dumbEvaluation ni = pure NetOutput
	{ noPriors = uniformPriors
	, noValuation = 0
	}

-- | @lerp alpha x y@ linearly interpolates between @x@ (when @alpha@ is @1@) and @y@ (when @alpha@ is @0@).
lerp :: Float -> Float -> Float -> Float
lerp alpha x y = alpha*x + (1-alpha)*y

-- | @\case 0 -> 1; v -> v@
unzero :: Float -> Float
unzero = \case 0 -> 1; v -> v

-- | Compute the AlphaZero variant of the predictor-biased upper confidence
-- bound score.
--
-- The first argument is a parameter that controls exploration, called c_{puct}
-- in Mastering the Game of Go Without Human Knowledge; larger values bias the
-- search more and more towards prior probabilities. Setting it to something
-- negative probably isn't sensible; it would cause the search to actively
-- avoid moves with high prior probabilities.
--
-- The remaining arguments are a prior probability for the current node, a
-- visit count for the parent node (n in the literature), a visit count for the
-- current node (n_i in the literature), and a cumulative utility achieved by
-- children of the current node (Q_i in the literature). The utility of
-- individual leaves should be in the range [0, 1] (so that 0 <= Q_i <= n_i).
pucb :: Float -> Float -> Float -> Float -> Float -> Float
pucb c_puct p n 0 _ = c_puct * p * sqrt n
pucb c_puct p n n_i q_i = q_i/n_i + c_puct * p * sqrt n / (1 + n_i)

maximumOn :: Ord a => (k -> v -> a) -> HashMap k v -> Maybe (k, v, a)
-- checking for emptiness once at the beginning is cheaper than re-checking on
-- every iteration, as you would have to do if you folded with a Maybe
maximumOn f m = case HM.toList m of
	[] -> Nothing
	(k,v):_ -> Just $ HM.foldlWithKey'
		(\old@(k,v,a) k' v' -> let a' = f k' v' in if a' > a then (k',v',a') else old)
		(k, v, f k v)
		m

ppRNGTree :: String -> RNGTree -> String
ppRNGTree indent t = ""
	++ ppPercent (priorProbabilityRNG t) ++ " "
	++ ppPrecision 2 (cumulativeValuationRNG t) ++ "/" ++ ppVisitCount (visitCountRNG t)
	++ ppMoveTrees indent (childrenRNG t)

ppRNGTreeDebug :: String -> RNGTree -> String
ppRNGTreeDebug indent t = ""
	++ ppPercent (priorProbabilityRNG t) ++ " "
	++ ppPrecision 2 (cumulativeValuationRNG t) ++ "/" ++ ppVisitCount (visitCountRNG t)
	++ "\n" ++ indent
	++ V.ifoldr
		(\i lk s -> (if i == nextLookaheadRNG t then \content -> "<" ++ content ++ ">" else id) (ppAeson lk) ++ " " ++ s)
		""
		(orderRNG t)
	++ "(" ++ show (nextLookaheadRNG t) ++ ")"
	++ "\n" ++ ppLabeledHashMap indent "children" ppMoveTreesDebug (childrenRNG t)
	++ "\n" ++ ppLabeledHashMap indent "placements" ppPlacements (placementsRNG t)
	++ "\n" ++ ppLabeledHashMap indent "symmetric placements" ppPlacements (symmetricPlacementsRNG t)
	++ "\n" ++ indent
	++ if childPriorsRNG t == uniformPriors
		then "priors: <uniform>"
		else "priors:" ++ "\n" ++ ppEndpointMap ("  " ++ indent) (childPriorsRNG t)

ppMoveTree :: String -> MoveTree -> String
ppMoveTree indent t = ppVisitCount (visitCountMove t) ++ case (children, unexplored) of
	("", "") -> " <empty>"
	("", _ ) -> labeledUnexplored
	(_ , "") -> newlinedChildren
	(_ , _ ) -> labeledUnexplored ++ newlinedChildren
	where
	children = ppHashMapInline' indent ppPill ppRNGTree (childrenMove t)
	unexplored = ppUnexploredMove (unexploredMove t)
	labeledUnexplored = "\n" ++ indent ++ "unexplored: " ++ elideTo 60 unexplored
	newlinedChildren = "\n" ++ children

ppMoveTreeDebug :: String -> MoveTree -> String
ppMoveTreeDebug indent t = ""
	++ indent ++ "visits: " ++ ppVisitCount (visitCountMove t) ++ "\n"
	++ (if null unexplored then "" else labeledUnexplored)
	++ (if null children then "" else labeledChildren)
	++ indent ++ "paths:\n" ++ ppHashMapInline ("  " ++ indent) ppPill ppAeson (pathsMove t)
	where
	children = ppHashMapInline' ("  " ++ indent) ppPill ppRNGTreeDebug (childrenMove t)
	unexplored = ppUnexploredMove (unexploredMove t)
	labeledChildren = indent ++ "children:\n" ++ children ++ "\n"
	labeledUnexplored = indent ++ "unexplored: " ++ unexplored ++ "\n"

elideTo :: Int -> String -> String
elideTo n s = case drop n s of
	[] -> s
	_ -> take (n-3) s ++ "..."

ppVisitCount :: Float -> String
ppVisitCount = show . round

ppMoveTrees :: String -> HashMap Lookahead MoveTree -> String
ppMoveTrees indent ts = if HM.null ts
	then ""
	else "\n" ++ ppHashMapInline' indent ppAeson ppMoveTree ts

ppLabeledHashMap :: String -> String -> (String -> HashMap k v -> String) -> HashMap k v -> String
ppLabeledHashMap indent label pp m = indent ++ label ++ ":" ++ if HM.null m
	then " <empty>"
	else "\n" ++ pp ("  " ++ indent) m

ppMoveTreesDebug :: String -> HashMap Lookahead MoveTree -> String
ppMoveTreesDebug indent = ppHashMap indent ppAeson ppMoveTreeDebug

ppPlacements :: String -> HashMap MidPlacement MidPath -> String
ppPlacements indent = ppHashMapInline indent ppMidPlacement ppAeson

ppEndpointMap :: String -> EndpointMap Pill CFloat -> String
ppEndpointMap indent m = intercalate "\n"
	[ indent ++ ppContent pc ++ ":\n" ++ ppRows ("  " ++ indent) pc
	| or <- [minBound .. maxBound]
	, bl <- [minBound .. maxBound]
	, oc <- [minBound .. maxBound]
	, let pc = PillContent
	      	{ orientation = or
	      	, bottomLeftColor = bl
	      	, otherColor = oc
	      	}
	] where
	ppRows indent' pc = intercalate "\n" [ppRow indent' pc y | y <- [15, 14..0]]
	ppRow indent' pc y = indent' ++ unwords
		[ show . round @Float . (*100) $ m EM.! Pill pc (Position x y)
		| x <- [0..7]
		]

ppHashMap :: String -> (k -> String) -> (String -> v -> String) -> HashMap k v -> String
ppHashMap indent ppk ppv m = intercalate "\n"
	[ indent ++ ppk k ++ ":\n" ++ ppv ("  " ++ indent) v
	| (k, v) <- HM.toList m
	]

ppMidPlacement :: MidPlacement -> String
ppMidPlacement (MidPlacement pos rot) = ppClockwiseRotations rot ++ " " ++ padr 7 (ppPosition pos)

ppClockwiseRotations :: Int -> String
ppClockwiseRotations = \case
	0 -> "  "
	1 -> " ↻"
	2 -> "↻↻"
	3 -> " ↺"
	_ -> "!!"

ppUnexploredMove :: Vector (Pill, Float) -> String
ppUnexploredMove ms = unwords [ppPill pill ++ "@" ++ ppPercent prior | (pill, prior) <- V.toList ms]

ppHashMapInline :: String -> (k -> String) -> (v -> String) -> HashMap k v -> String
ppHashMapInline indent ppk ppv m = intercalate "\n"
	[ indent ++ ppk k ++ " ↦ " ++ ppv v
	| (k, v) <- HM.toList m
	]

ppHashMapInline' :: String -> (k -> String) -> (String -> v -> String) -> HashMap k v -> String
ppHashMapInline' indent ppk ppv m = intercalate "\n"
	[ indent ++ ppk k ++ ": " ++ ppv deeper v
	| (k, v) <- HM.toList m
	] where deeper = "  " ++ indent

ppAeson :: ToJSON a => a -> String
ppAeson a = case toJSON a of
	String t -> T.unpack t
	other -> LBS8.unpack (encode other)

-- all the rest of this stuff is just for debugging
ppPill :: Pill -> String
ppPill p = ppContent (content p) ++ "@" ++ ppPosition (bottomLeftPosition p)

ppContent :: PillContent -> String
ppContent pc = [ppOrientation (orientation pc), ppColor (bottomLeftColor pc), ppColor (otherColor pc)]

ppOrientation :: Orientation -> Char
ppOrientation Horizontal = '↔'
ppOrientation Vertical = '↕'

ppColor :: Color -> Char
ppColor Blue = 'b'
ppColor Red = 'r'
ppColor Yellow = 'y'

ppPosition :: Position -> String
ppPosition pos = "(" ++ show (x pos) ++ ", " ++ show (y pos) ++ ")"

ppPercent :: Float -> String
ppPercent p = (if isNaN p then "nan" else show (round (100*p))) ++ "%"

ppPrecision :: Int -> Float -> String
ppPrecision p n = if isNaN n then "nan" else showFFloat Nothing (fromInteger (round (pow*n))/pow) ""
	where pow = 10^p

padr :: Int -> String -> String
padr n s = s ++ replicate (n - length s) ' '
