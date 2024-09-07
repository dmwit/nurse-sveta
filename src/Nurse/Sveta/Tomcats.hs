module Nurse.Sveta.Tomcats (
	SearchContext(..),
	RNGTree(..), MoveTree(..),
	newRNGTree, newRNGTreeFromSeed, newMoveTree,
	descendRNGTree, descendMoveTree,
	expandRNGTree', expandRNGTree, expandMoveTree', expandMoveTree,
	DMParameters, dmParameters,
	GameStateSeed(..), initialTree,
	mcts, descend, unsafeDescend,
	evaluateFinalState, netInput, dumbEvaluation,
	dmFinished, dmPlay, approximateCostModel,
	SearchConfiguration(..),
	Move(..),
	GameState(..),
	Statistics(..),
	Tree(..),
	clone,
	maximumOn, BestMoves(..), bestMoves,
	ppTreeIO, ppTreeSparseIO,
	ppRNGTree, ppRNGTreeDebug, ppMoveTree, ppMoveTreeDebug,
	ppMoveTrees, ppPlacements, ppEndpointMap, ppUnexploredMove,
	ppAeson,
	ppTreeSparse, ppTreesSparse, ppTree, ppTrees,
	ppStats, ppStatss, ppCache,
	ppMove,
	ppPill, ppContent, ppOrientation, ppColor,
	ppPosition,
	ppPercent, ppPrecision,
	) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Aeson
import Data.Bits
import Data.Foldable
import Data.Functor
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.List
import Data.Semigroup
import Data.Traversable
import Data.Vector (Vector)
import Dr.Mario.Model
import Dr.Mario.Pathfinding
import Numeric
import Nurse.Sveta.STM.BatchProcessor
import Nurse.Sveta.Torch.Semantics
import System.Random.MWC
import System.Random.MWC.Distributions
import System.Random.Stateful (uniformDouble01M)
import System.IO.Unsafe (unsafeInterleaveIO)
import Tomcats hiding (descend)
import Tomcats.AlphaZero.Float (Statistics(..))

import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Nurse.Sveta.Torch.EndpointMap as EM
import qualified Data.Text as T
import qualified Tomcats as T
import qualified Tomcats.AlphaZero.Float as A0

-- TODO:
-- 	* board/pill generation options like MWC vs LFSR, Algorithm X vs NES' algo,
-- 	  128 pre-determined pills vs on-the-fly, maybe SNES vs NES distro?
-- 	* neural net vs. not
data SearchConfiguration = SearchConfiguration
	{ c_puct :: Float
	, iterations :: Int
	, typicalMoves :: Float
	, priorNoise :: Float
	, moveNoise :: Double -- ^ probability of not choosing the best move
	} deriving (Eq, Ord, Read, Show)

-- | The 'MidPath' is ignored for 'Eq', 'Ord', and 'Hashable'.
data Move = RNG Color Color | Placement MidPath Pill deriving (Show, Read)

instance Eq Move where
	RNG l0 r0 == RNG l1 r1 = l0 == l1 && r0 == r1
	Placement _ l == Placement _ r = l == r
	_ == _ = False

instance Ord Move where
	compare (RNG l0 l1) (RNG r0 r1) = compare l0 r0 <> compare l1 r1
	compare (Placement _ l) (Placement _ r) = compare l r
	compare RNG{} _ = LT
	compare Placement{} _ = GT

instance Hashable Move where
	hashWithSalt s = \case
		RNG c c'         ->            s `hashWithSalt` c `hashWithSalt` c'
		Placement _ pill -> complement s `hashWithSalt` pill

instance ToJSON Move where
	toJSON = \case
		RNG l r -> toJSON [l, r]
		Placement m p -> toJSON (m, p)
	toEncoding = \case
		RNG l r -> toEncoding [l, r]
		Placement m p -> toEncoding (m, p)

instance FromJSON Move where
	parseJSON v = parseRNG <|> parsePlacement where
		parseRNG = do
			[l, r] <- parseJSON v
			pure (RNG l r)
		parsePlacement = uncurry Placement <$> parseJSON v

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

chooseRNG :: GameState -> Lookahead -> IO ()
chooseRNG = writeIORef . lookbehind

chooseMove :: GameState -> MidPath -> Pill -> IO (Maybe ClearResults)
chooseMove gs path pill = do
	mres <- mplaceDetails (board gs) pill
	mres <$ for_ mres \results -> do
		let counts = summarizeClearResults results
		-- defensive programming: these should naturally get forced during
		-- position evaluation, but why take the risk? let's just keep the
		-- thunk depth low
		modifyIORef' (virusesKilled gs) (clears counts +)
		modifyIORef' (pillsUsed gs) succ
		modifyIORef' (framesPassed gs) (approximateCostModel path pill counts +)

type DMParameters = Parameters IO Float Statistics Move GameState

class GameStateSeed a where initialState :: a -> IO GameState

instance Gen a ~ GenIO => GameStateSeed (Gen a) where
	-- note to self: when the NES does it, it generates the pill sequence first,
	-- then the board
	initialState g = do
		level <- uniformRM (0, 20) g
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
	{ ctxEval :: Procedure NetInput NetOutput'
	, ctxRNG :: GenIO
	, ctxState :: GameState
	, ctxDiscountRate :: Float
	, ctxC_puct :: Float
	, ctxRewardVirusClear :: Float
	, ctxRewardOtherClear :: Float
	, ctxRewardWin :: Float
	, ctxRewardLoss :: Float
	}

newRNGTree :: SearchContext -> Float -> IO RNGTree
newRNGTree ctx prior = snd <$> newRNGTreeFromSeed (ctxEval ctx) (ctxRNG ctx) prior (ctxState ctx)

newRNGTree' :: SearchContext -> Float -> IO (Float, RNGTree)
newRNGTree' ctx prior = (\t -> (cumulativeValuationRNG t, t)) <$> newRNGTree ctx prior

-- TODO: could return an IO (GameState, IO RNGTree) to let the caller choose when to wait on the net evaluation
newRNGTreeFromSeed :: GameStateSeed a => Procedure NetInput NetOutput' -> GenIO -> Float -> a -> IO (GameState, RNGTree)
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
	    	, childPriorsRNG = noPriors' no
	    	, priorProbabilityRNG = prior
	    	, cumulativeValuationRNG = noValuation' no
	    	, visitCountRNG = 1
	    	}

	pure (gs, t)
	where
	symmetrize :: HashMap MidPlacement MidPath -> HashMap MidPlacement MidPath
	symmetrize moves = HM.fromListWith shorterPath [(placement { mpRotations = mpRotations placement .&. 1 }, path) | (placement, path) <- HM.toList moves]

newMoveTree :: SearchContext -> HashMap MidPlacement MidPath -> EndpointMap Pill CFloat -> Lookahead -> IO MoveTree
newMoveTree ctx pathCache priors lk = dmFinished (ctxState ctx) <&> \finished -> MoveTree
	{ childrenMove = HM.empty
	, unexploredMove = if finished then V.empty else unexplored
	, pathsMove = if finished then HM.empty else paths
	, visitCountMove = 0
	}
	where
	unexplored = V.fromList $ sortBy
		(\(pill, prior) (pill', prior') -> compare prior' prior <> compare pill pill')
		[(pill, (priors EM.! pill) / A0.unzero normalizationFactor) | pill <- HM.keys paths]
	paths = HM.fromListWithKey (\pill -> error $ "Two different MidPlacements both got specialized to " ++ show pill ++ ". Original path cache: " ++ show pathCache)
		[ (mpPill placement lk, path)
		| (placement, path) <- HM.toList pathCache
		]
	Sum normalizationFactor = HM.foldMapWithKey (\pill _ -> Sum (priors EM.! pill)) paths

-- | modifies the GameState
descendRNGTree :: SearchContext -> RNGTree -> Lookahead -> IO MoveTree
descendRNGTree ctx t lk = do
	chooseRNG (ctxState ctx) lk
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
	Just path -> chooseMove (ctxState ctx) path pill >>= \case
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
	(Nothing, Nothing) -> flip (,) t { visitCountMove = visitCountMove t + 1 } <$> finalReward ctx
	where
	scoreTree t' = A0.pucbA0Raw (ctxC_puct ctx) (priorProbabilityRNG t') (visitCountMove t) (visitCountRNG t') (cumulativeValuationRNG t')
	scorePrior (_, prior) = A0.pucbA0Raw (ctxC_puct ctx) prior (visitCountMove t) 0 0

	explore = let (pill, prior) = V.head (unexploredMove t) in go pill (newRNGTree' ctx prior) t { unexploredMove = V.tail (unexploredMove t) }
	exploit pill t' = go pill (expandRNGTree' ctx t') t
	go pill buildTree t' = do
		before <- readIORef (framesPassed (ctxState ctx))
		Just results <- chooseMove (ctxState ctx) (pathsMove t HM.! pill) pill
		after <- readIORef (framesPassed (ctxState ctx))
		(dv, tNew) <- buildTree
		pure ( immediateReward ctx results + ctxDiscount ctx (after - before) dv
		     , t' { childrenMove = HM.insert pill tNew (childrenMove t'), visitCountMove = visitCountMove t' + 1 }
		     )

withClonedState :: (SearchContext -> a -> IO b) -> SearchContext -> a -> IO b
withClonedState f ctx a = do
	gs <- dmClone (ctxState ctx)
	f ctx { ctxState = gs } a

-- | does not modify the GameState
expandRNGTree :: SearchContext -> RNGTree -> IO RNGTree
expandRNGTree ctx t = snd <$> withClonedState expandRNGTree' ctx t

-- | does not modify the GameState
expandMoveTree :: SearchContext -> MoveTree -> IO MoveTree
expandMoveTree ctx t = snd <$> withClonedState expandMoveTree' ctx t

ctxDiscount :: SearchContext -> Int -> Float -> Float
ctxDiscount ctx n v = v * ctxDiscountRate ctx ^ n

ctxRewardClear :: SearchContext -> Shape -> Float
ctxRewardClear ctx = \case
	Virus -> ctxRewardVirusClear ctx
	_ -> ctxRewardOtherClear ctx

immediateReward :: SearchContext -> ClearResults -> Float
immediateReward ctx = goClear where
	goClear = \case
		NoClear -> 0
		Clear cs dRes -> getSum (foldMap rewardClear cs) + discount clearCost (goDrop dRes)
	goDrop = \case
		NoDrop -> 0
		Drop ds cRes -> discount (getMax (foldMap (Max . fst) ds) * dropCost) (goClear cRes)
	rewardClear = Sum . ctxRewardClear ctx . oshape
	discount = ctxDiscount ctx

-- | does not modify the GameState
finalReward :: SearchContext -> IO Float
finalReward ctx = do
	clearedViruses <- readIORef (virusesKilled (ctxState ctx))
	pure if clearedViruses == originalVirusCount (ctxState ctx)
		then ctxRewardWin ctx
		else ctxRewardLoss ctx

allLookaheads :: Vector Lookahead
-- could use EndpointKey to not have constants 9 and 3 here, but the result is completely unreadable
allLookaheads = V.generate (product (indexCounts' @Lookahead)) fromIndex

uniformPriors :: EndpointMap Pill CFloat
uniformPriors = EM.EndpointMap (foldr (svReplicate . evalGameConstant) (generate [] \_ -> 1) (indexCounts @Pill))

--- begin PP
ppRNGTree :: String -> RNGTree -> String
ppRNGTree indent t = indent
	++ ppPercent (priorProbabilityRNG t) ++ " "
	++ ppPrecision 2 (cumulativeValuationRNG t) ++ "/" ++ show (round (visitCountRNG t))
	++ "\n" ++ ppMoveTrees indent (childrenRNG t)

ppRNGTreeDebug :: String -> RNGTree -> String
ppRNGTreeDebug indent t = indent
	++ ppPercent (priorProbabilityRNG t) ++ " "
	++ ppPrecision 2 (cumulativeValuationRNG t) ++ "/" ++ show (round (visitCountRNG t))
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
ppMoveTree indent t = case (children, unexplored) of
	("", "") -> indent ++ "<empty>"
	("", _ ) -> labeledUnexplored
	(_ , "") -> labeledChildren
	(_ , _ ) -> labeledUnexplored ++ "\n" ++ labeledChildren
	where
	children = ppHashMap ("  " ++ indent) ppPill ppRNGTree (childrenMove t)
	unexplored = ppUnexploredMove (unexploredMove t)
	labeledChildren = indent ++ "children:\n" ++ children
	labeledUnexplored = indent ++ "unexplored: " ++ unexplored

ppMoveTreeDebug :: String -> MoveTree -> String
ppMoveTreeDebug indent t = ""
	++ (if null unexplored then "" else labeledUnexplored)
	++ (if null children then "" else labeledChildren)
	++ indent ++ "paths:\n" ++ ppHashMapInline ("  " ++ indent) ppPill ppAeson (pathsMove t)
	where
	children = ppHashMap ("  " ++ indent) ppPill ppRNGTreeDebug (childrenMove t)
	unexplored = ppUnexploredMove (unexploredMove t)
	labeledChildren = indent ++ "children:\n" ++ children ++ "\n"
	labeledUnexplored = indent ++ "unexplored: " ++ unexplored ++ "\n"

ppMoveTrees :: String -> HashMap Lookahead MoveTree -> String
ppMoveTrees indent ts = if HM.null ts
	then indent ++ "(no children)"
	else ppHashMap ("  " ++ indent) ppAeson ppMoveTree ts

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

ppAeson :: ToJSON a => a -> String
ppAeson a = case toJSON a of
	String t -> T.unpack t
	other -> LBS8.unpack (encode other)

padr :: Int -> String -> String
padr n s = s ++ replicate (n - length s) ' '

--- end PP

initialTree :: GameStateSeed a => DMParameters -> a -> IO (GameState, Tree Statistics Move)
initialTree params g = do
	s <- initialState g
	(_, t) <- Tomcats.initialize params s >>= preprocess params s
	pure (s, t)

dmParameters :: SearchConfiguration -> Procedure NetInput NetOutput -> GenIO -> DMParameters
dmParameters config eval gen = Parameters
	{ score = dmScore config
	, expand = dmExpand gen
	, clone = dmClone
	, play = dmPlay
	, preprocess = dmPreprocess config eval gen
	}

dmScore :: SearchConfiguration -> Move -> Statistics -> Statistics -> Float
-- We should probably avoid looking at RNG moves in the same order every time,
-- as that could introduce a bias. In dmExpand, we put a bit of randomness
-- into the priors, which we can use to break ordering ties here.
dmScore _ RNG{} _ stats = priorProbability stats - visitCount stats
dmScore config _ parent child = A0.pucbA0Raw (c_puct config) (priorProbability child) (visitCount parent) (visitCount child) (cumulativeValuation child)

dmFinished :: GameState -> IO Bool
dmFinished gs = do
	cellL <- mget (board gs) startingBottomLeftPosition
	cellR <- mget (board gs) startingOtherPosition
	remaining <- (originalVirusCount gs-) <$> readIORef (virusesKilled gs)
	pure $ cellL /= Just Empty || cellR /= Just Empty || remaining <= 0

-- score: 0 or  1/3 for finishing
--        up to 1/3 for each virus cleared; want the bot to learn early that clearing is good, so reward heavily for the first few viruses
--        up to 1/3 for clearing quickly if you win; the quicker you get, the harder it is to get quicker, so increase the reward more quickly when it's fast
baseWinningValuation :: Int -> Int -> Float
baseWinningValuation frames_ ogViruses = 1 - sqrt (min 1 (frames / maxFrames)) / 3 where
	frames = fromIntegral (max 1 frames_)
	maxFrames = fromIntegral (shiftL ogViruses 9)

winningValuation :: GameState -> IO Float
winningValuation gs = do
	frames <- readIORef (framesPassed gs)
	pure $ baseWinningValuation frames (originalVirusCount gs)

baseLosingValuation :: Int -> Int -> Float
baseLosingValuation cleared original = sqrt (fromIntegral cleared / fromIntegral original) / 3

losingValuation :: GameState -> IO Float
losingValuation gs = do
	clearedViruses <- readIORef (virusesKilled gs)
	pure $ baseLosingValuation clearedViruses (originalVirusCount gs)

evaluateFinalState :: GameState -> IO Float
evaluateFinalState gs = do
	clearedViruses <- readIORef (virusesKilled gs)
	if clearedViruses == originalVirusCount gs
		then winningValuation gs
		else losingValuation gs

niEvaluateFinalState :: NetInput -> Float
niEvaluateFinalState ni = if remaining == 0
	then baseWinningValuation (niFrames ni) orig
	else baseLosingValuation (orig - remaining) orig
	where
	orig = niOriginalVirusCount ni
	Sum remaining = ofoldMap isVirus (niBoard ni)
	isVirus = \case
		Occupied _ Virus -> 1
		_ -> 0

-- We should probably avoid looking at RNG moves in the same order every time,
-- as that could introduce a bias. So we toss a tiny tiny bit of randomness
-- into the priors.
--
-- Because of the way the preprocessor is set up -- to always convert all
-- unexplored RNG nodes to child RNG nodes immediately with a shared
-- pathfinding result -- this expansion is only ever called right after we
-- place a pill, so we can always produce RNG moves.
dmExpand :: GenIO -> GameState -> IO (Statistics, HashMap Move Statistics)
dmExpand = \gen gs -> dmFinished gs >>= \case
	False -> do
		perm <- uniformPermutation n gen
		let mk i [l, r] = (RNG l r, Statistics 0 (1/fromIntegral n + fromIntegral (perm V.! i - halfn) * 1e-8) 0)
		-- The neural net takes the lookahead pill as part of its input. Since
		-- we haven't decided on that yet, we can't actually report back its
		-- evaluation of the current state here. So we report zero visits and
		-- zero value, and fix this up in the preprocessor once we actually
		-- choose a next pill. Often we will descend into the same part of the
		-- tree in the next iteration or two, and fix it up fairly promptly --
		-- though that's not guaranteed!
		pure (mempty, HM.fromList (zipWith mk [0..] (replicateM 2 colors)))
	True -> evaluateFinalState gs <&> \score -> (Statistics 1 0 score, HM.empty)
	where
	n = length colors^2
	halfn = n `div` 2
	colors = [minBound..maxBound :: Color]

dmClone :: GameState -> IO GameState
dmClone gs = pure GameState
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

dmPlay :: GameState -> Move -> IO ()
dmPlay gs = \case
	RNG l r -> chooseRNG gs (Lookahead l r)
	Placement path pill -> () <$ chooseMove gs path pill

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

dmPreprocess :: SearchConfiguration -> Procedure NetInput NetOutput -> GenIO -> GameState -> Tree Statistics Move -> IO (Statistics, Tree Statistics Move)
dmPreprocess config eval gen gs t
	| HM.null (children t)
	, RNG{}:_ <- HM.keys (unexplored t) = do
		future <- schedule eval =<< netInput gs
		fp <- readIORef (framesPassed gs)
		pu <- readIORef (pillsUsed gs)
		movesHM <- mapproxReachable (board gs) (fp .&. 1 /= fromEnum (originalSensitive gs)) (gravity (speed gs) pu)
		let symmetricMovesHM = HM.fromListWith shorterPath [(placement { mpRotations = mpRotations placement .&. 1 }, path) | (placement, path) <- moves]
		    moves = HM.toList movesHM
		    symmetricMoves = HM.toList symmetricMovesHM
		allNoise <- sequence
			[ fmap ((,) lk) . A0.dirichlet (10/A0.unzero (typicalMoves config)) const gen $ HM.fromListWithKey deduplicationError
				[ (Placement path pill, ())
				| (placement, path) <- if l == r then symmetricMoves else moves
				, let pill = mpPill placement lk
				]
			| l <- [minBound .. maxBound]
			, r <- [minBound .. maxBound]
			, let lk = Lookahead l r
			]
		no <- future
		let cs = HM.fromList
		    	[ (,) mv Tree
		    		{ statistics = (unexplored t HM.! mv)
		    			{ visitCount = 1
		    			, cumulativeValuation = noValuation no HM.! lk
		    			}
		    		, children = HM.empty
		    		, unexplored = lerpNoise . A0.normalizeStatistics $ flip HM.mapWithKey lkNoise \(Placement path pill) noise ->
		    			Statistics
		    				{ visitCount = 0
		    				, priorProbability = noPriors no HM.! pill
		    				-- we smuggle the noise out through cumulativeValuation
		    				-- lerpNoise will mix this noise with the normalized prior, then zero out the cumulativeValuation
		    				, cumulativeValuation = noise
		    				}
		    		, cachedEvaluation = Nothing
		    		}
		    	| (lk@(Lookahead l r), lkNoise) <- allNoise
		    	, let mv = RNG l r
		    	]
		    stats = (foldMap' statistics cs) { priorProbability = 0 }
		pure (stats, Tree
			{ statistics = stats <> statistics t
			, children = cs
			, unexplored = HM.empty
			, cachedEvaluation = Nothing
			})
	| otherwise = pure (mempty, t)
	where
	deduplicationError k p1 p2 = error . unwords $ tail [undefined
		, "dmPreprocess: found duplicate paths (there is a deduplication phase that should have already ruled this out);"
		, "guru meditation"
		, show (k, p1, p2)
		]
	lerpNoise = fmap \stats -> stats
		{ priorProbability = A0.lerp (priorNoise config) (cumulativeValuation stats) (priorProbability stats)
		, cumulativeValuation = 0
		}

-- TODO: perhaps one day we should think about how to fuse this with toEndpoint
netInput :: GameState -> IO NetInput
netInput s = pure NetInput
	<*> mfreeze (board s)
	<*> readIORef (framesPassed s)
	<*> pure (originalVirusCount s)

dumbEvaluation :: NetInput -> IO NetOutput
dumbEvaluation = \ni -> pure NetOutput
	{ noPriors = uniformPriors
	, noValuation = niEvaluateFinalState ni <$ zeroValuation
	} where
	zeroValuation = HM.fromList
		[ (Lookahead { leftColor = l , rightColor = r }, 0)
		| l <- [minBound .. maxBound]
		, r <- [minBound .. maxBound]
		]
	uniformPriors = HM.fromList
		[(,) Pill
			{ content = PillContent
				{ bottomLeftColor = bl
				, otherColor = oc
				, orientation = o
				}
			, bottomLeftPosition = Position x y
			}
			1
		| bl <- [minBound .. maxBound]
		, oc <- [minBound .. maxBound]
		, o <- [minBound .. maxBound]
		, x <- [0..case o of Horizontal -> 6; _ -> 7]
		, y <- [0..15]
		]

-- With probability p, choose the move with the most visits. If that doesn't
-- happen, with probability p, choose using the visit counts as weights for a
-- (lightly smoothed) categorical distribution. Otherwise choose completely
-- uniformly at random.
--
-- All the complicated bits are just about handling edge cases gracefully.
descend :: GenIO -> SearchConfiguration -> DMParameters -> GameState -> Tree Statistics Move -> IO (Maybe (Move, Tree Statistics Move))
descend g config params s t = do
	v <- uniformDouble01M g
	mmove <- case (moves, v < moveNoise config^2, v < moveNoise config) of
		([], _    , _    ) -> pure Nothing
		(_ , True , _    ) -> Just . (moves!!) <$> uniformR (0, length moves - 1) g
		(_ , _    , True ) -> Just . (moves!!) <$> categorical (V.fromList weights) g
		_ -> case bestMoves t of
			BestMoves _ ms | V.length ms == 0 -> Just . (moves!!) <$> uniformR (0, length moves - 1) g
			               | otherwise        -> Just . (ms  V.!) <$> uniformR (0, V.length ms - 1) g
	for mmove \move -> do
		play params s move
		case HM.lookup move (children t) of
			Just t' -> pure (move, t')
			Nothing -> (,) move . snd <$> initialTree params s
	where
	weight = realToFrac . (1+) . visitCount
	(moves, weights) = unzip . HM.toList $
		(weight . statistics <$> children t) `HM.union`
		(weight <$> unexplored t)

data BestMoves = BestMoves Float (Vector Move) deriving (Eq, Ord, Read, Show)

instance Semigroup BestMoves where
	bm@(BestMoves n mvs) <> bm'@(BestMoves n' mvs') = case compare n n' of
		LT -> bm'
		EQ -> BestMoves n (mvs <> mvs')
		GT -> bm

instance Monoid BestMoves where mempty = BestMoves (-1/0) V.empty

injBestMoves :: Move -> Tree Statistics Move -> BestMoves
injBestMoves mv t = BestMoves (visitCount (statistics t)) (V.singleton mv)

bestMoves :: Tree Statistics Move -> BestMoves
bestMoves = HM.foldMapWithKey injBestMoves . children

-- all the rest of this stuff is just for debugging

ppTreeSparseIO :: Tree Statistics Move -> IO ()
ppTreeSparseIO = putStrLn . ppTreeSparse ""

ppTreeIO :: Tree Statistics Move -> IO ()
ppTreeIO = putStrLn . ppTree ""

ppTreeSparse :: String -> Tree Statistics Move -> String
ppTreeSparse indent (Tree stats cs un cache) = ""
	++ ppStats stats ++ "{"
	++ case ppTreesSparse ('\t':indent) cs of
	   	"" -> ""
	   	s -> "\n" ++ s ++ indent
	++ "}" ++ ppCache cache

ppTreesSparse :: String -> HashMap Move (Tree Statistics Move) -> String
ppTreesSparse indent ts = concat
	[ indent ++ ppMove move ++ "↦" ++ ppTreeSparse indent t ++ ",\n"
	| (move, t) <- HM.toList ts
	, visitCount (statistics t) > 0
	]

ppTree :: String -> Tree Statistics Move -> String
ppTree indent (Tree stats cs un cache) = ppStats stats ++ "{" ++ rec ++ "}" ++ ppCache cache
	where
	indent' = '\t':indent
	rec = case (ppTrees indent' cs, ppStatss un) of
		("", "") -> ""
		(s, "") -> "\n" ++ s ++ indent
		(s, s') -> "\n" ++ s ++ indent' ++ s' ++ "\n" ++ indent

ppStats :: Statistics -> String
ppStats (Statistics visits prob val) = ppPercent prob ++ " " ++ ppPrecision 2 val ++ "/" ++ show (round visits)

ppTrees :: String -> HashMap Move (Tree Statistics Move) -> String
ppTrees indent ts = concat
	[ indent ++ ppMove move ++ "↦" ++ ppTree indent t ++ ",\n"
	| (move, t) <- HM.toList ts
	]

ppStatss :: HashMap Move Statistics -> String
ppStatss ss = concat
	[ ppMove move ++ "⇨" ++ ppStats stats ++ ", "
	| (move, stats) <- HM.toList ss
	]

ppCache :: Maybe Statistics -> String
ppCache Nothing = ""
ppCache (Just stats) = "~" ++ ppPrecision 2 (cumulativeValuation stats)

ppMove :: Move -> String
ppMove (RNG l r) = [ppColor l, ppColor r]
ppMove (Placement bm pill) = ppPill pill

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
