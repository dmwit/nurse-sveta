module Nurse.Sveta.Tomcats (
	DMParameters, dmParameters,
	GameStateSeed(..), initialTree,
	mcts, descend, unsafeDescend,
	evaluateFinalState, DetailedEvaluation, dumbEvaluation,
	dmFinished, dmPlay, approximateCostModel,
	SearchConfiguration(..),
	Move(..),
	GameState(..),
	A0.Statistics(..),
	Tree(..),
	clone,
	maximumOn,
	) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Aeson
import Data.Bits
import Data.Functor
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Monoid
import Data.Vector (Vector)
import Dr.Mario.Model
import Dr.Mario.Pathfinding
import Nurse.Sveta.STM.BatchProcessor
import System.Random.MWC
import System.Random.MWC.Distributions
import System.IO.Unsafe (unsafeInterleaveIO)
import Tomcats

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Tomcats.AlphaZero as A0

-- TODO:
-- 	* board/pill generation options like MWC vs LFSR, Algorithm X vs NES' algo,
-- 	  128 pre-determined pills vs on-the-fly, maybe SNES vs NES distro?
-- 	* neural net vs. not
-- 	* temperature
data SearchConfiguration = SearchConfiguration
	{ c_puct :: Double
	, iterations :: Int
	, typicalMoves :: Double
	, priorNoise :: Double
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
	, lookbehind :: IORef (Color, Color)
	, originalVirusCount :: Int
	, originalSensitive :: Bool
	, speed :: CoarseSpeed
	}

uninitializedLookbehind :: IO (IORef (Color, Color))
uninitializedLookbehind = newIORef (error "asked what pill was last launched before any pill was launched")

type DMParameters = Parameters IO Double A0.Statistics Move GameState

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

initialTree :: GameStateSeed a => DMParameters -> a -> IO (GameState, Tree A0.Statistics Move)
initialTree params g = do
	s <- initialState g
	(_, t) <- Tomcats.initialize params s >>= preprocess params s
	pure (s, t)

dmParameters :: SearchConfiguration -> Procedure GameState DetailedEvaluation -> GenIO -> DMParameters
dmParameters config eval gen = Parameters
	{ score = dmScore config
	, expand = dmExpand gen
	, clone = dmClone
	, play = dmPlay
	, preprocess = dmPreprocess config eval gen
	}

dmScore :: SearchConfiguration -> Move -> A0.Statistics -> A0.Statistics -> Double
-- We should probably avoid looking at RNG moves in the same order every time,
-- as that could introduce a bias. In dmExpand, we put a bit of randomness
-- into the priors, which we can use to break ordering ties here.
dmScore _ RNG{} _ stats = A0.priorProbability stats - A0.visitCount stats
dmScore config _ parent child = pucbA0 (c_puct config) (A0.priorProbability child) (A0.visitCount parent) (A0.visitCount child) (A0.cumulativeValuation child)

dmFinished :: GameState -> IO Bool
dmFinished gs = do
	cellL <- mget (board gs) startingBottomLeftPosition
	cellR <- mget (board gs) startingOtherPosition
	remaining <- (originalVirusCount gs-) <$> readIORef (virusesKilled gs)
	pure $ cellL /= Just Empty || cellR /= Just Empty || remaining <= 0

-- score: 0 or  1/3 for finishing
--        up to 1/3 for each virus cleared; want the bot to learn early that clearing is good, so reward heavily for the first few viruses
--        up to 1/3 for clearing quickly if you win; the quicker you get, the harder it is to get quicker, so increase the reward more quickly when it's fast
evaluateFinalState :: GameState -> IO Double
evaluateFinalState gs = do
	clearedViruses <- readIORef (virusesKilled gs)
	frames_ <- readIORef (framesPassed gs)
	let origViruses = originalVirusCount gs
	    frames = fromIntegral (max 1 frames_)
	    maxFrames = fromIntegral (shiftL origViruses 9)
	pure $ if clearedViruses == origViruses
		then 1 - sqrt (min 1 (frames / maxFrames)) / 3
		else sqrt (fromIntegral clearedViruses / fromIntegral origViruses) / 3

-- We should probably avoid looking at RNG moves in the same order every time,
-- as that could introduce a bias. So we toss a tiny tiny bit of randomness
-- into the priors.
--
-- Because of the way the preprocessor is set up -- to always convert all
-- unexplored RNG nodes to child RNG nodes immediately with a shared
-- pathfinding result -- this expansion is only ever called right after we
-- place a pill, so we can always produce RNG moves.
dmExpand :: GenIO -> GameState -> IO (A0.Statistics, HashMap Move A0.Statistics)
dmExpand = \gen gs -> dmFinished gs >>= \case
	False -> do
		perm <- uniformPermutation n gen
		let mk i [l, r] = (RNG l r, A0.Statistics 0 (1/fromIntegral n + fromIntegral (perm V.! i - halfn) * 1e-8) 0)
		pure (mempty, HM.fromList (zipWith mk [0..] (replicateM 2 colors)))
	True -> evaluateFinalState gs <&> \score -> (singleVisitStats score, HM.empty)
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
	RNG l r -> writeIORef (lookbehind gs) (l, r)
	Placement path pill -> mplace (board gs) pill >>= \case
		Nothing -> pure ()
		Just counts ->  do
			-- defensive programming: these should naturally get forced during
			-- position evaluation, but why take the risk? let's just keep the
			-- thunk depth low
			modifyIORef' (virusesKilled gs) (clears counts +)
			modifyIORef' (pillsUsed gs) succ
			modifyIORef' (framesPassed gs) (approximateCostModel path pill counts +)

yCosts :: V.Vector Int
yCosts = V.fromList [47 {- TODO -}, 46 {- TODO -}, 44, 42, 41, 41, 39, 39, 37, 37, 35, 35, 33, 34, 34, 34]

-- TODO: figure out the exact cost model for the NES, then move that into maryodel
-- TODO: don't count fall time after the last clear
approximateCostModel :: MidPath -> Pill -> CleanupResults -> Int
approximateCostModel move pill counts = 0
	+ yCosts V.! y (bottomLeftPosition pill) -- throwing animation, lock animation, and pause between lock and next throw
	+ mpPathLength move -- pill maneuvering
	+ sum [16*n + 20 | n <- rowsFallen counts] -- fall time + clear animation

dmPreprocess :: SearchConfiguration -> Procedure GameState DetailedEvaluation -> GenIO -> GameState -> Tree A0.Statistics Move -> IO (A0.Statistics, Tree A0.Statistics Move)
dmPreprocess config eval gen gs t
	| HM.null (children t) = case HM.keys (unexplored t) of
		RNG{}:_ -> do
			fp <- readIORef (framesPassed gs)
			pu <- readIORef (pillsUsed gs)
			moves <- HM.toList <$> mapproxReachable (board gs) (fp .&. 1 /= fromEnum (originalSensitive gs)) (gravity (speed gs) pu)
			let symmetricMoves = HM.toList $ HM.fromListWith shorterPath [(placement { mpRotations = mpRotations placement .&. 1 }, path) | (placement, path) <- moves]
			pure . (,) mempty $ t
				{ unexplored = HM.empty
				, children = flip HM.mapWithKey (unexplored t) $ \(RNG l r) stats -> Tree
					{ statistics = stats
					, children = HM.empty
					, cachedEvaluation = Nothing
					, unexplored = HM.fromListWithKey deduplicationError
						[ (Placement path (mpPill placement l r), mempty)
						| (placement, path) <- if l == r then symmetricMoves else moves
						]
					}
				}
		Placement{}:_ -> do
			(valueEstimate, moveWeights) <- call eval gs
			let stats = singleVisitStats valueEstimate
			priors <- id
				. A0.dirichletA0 (typicalMoves config) (priorNoise config) gen
				. A0.normalize
				. HM.mapWithKey (\(Placement _ (Pill pc bl)) _ -> moveWeights HM.! pc V.! x bl V.! y bl)
				$ unexplored t
			pure (stats, t { statistics = statistics t <> stats, unexplored = priors })
		[] -> pure (mempty, t)
	| otherwise = pure (mempty, t)
	where
	deduplicationError k p1 p2 = error . unwords $ tail [undefined
		, "dmPreprocess: found duplicate paths (there is a deduplication phase that should have already ruled this out);"
		, "guru meditation"
		, show (k, p1, p2)
		]

singleVisitStats :: Double -> A0.Statistics
singleVisitStats = A0.Statistics 1 0

type DetailedEvaluation = (Double, HashMap PillContent (Vector (Vector Double)))

-- | Given the game state and the colors for the upcoming pill, guess where
-- moves will be made and how good the final outcome will be. The Vector's are
-- indexed by (x, y) position of the bottom left.
dumbEvaluation :: GameState -> IO DetailedEvaluation
dumbEvaluation = \s -> do
	(l, r) <- readIORef (lookbehind s)
	points <- evaluateFinalState s
	pure (points, HM.fromList
		[ (PillContent orient bl o, vec)
		| orient <- [Horizontal, Vertical]
		, (bl, o) <- [(l, r), (r, l)]
		])
	-- when horizontal, this has one extra x position. but who cares?
	where vec = V.replicate 8 (V.replicate 16 1)

-- all the rest of this stuff is just for debugging

ppTreeSparse :: String -> Tree A0.Statistics Move -> String
ppTreeSparse indent (Tree stats cs un cache) = ppStats stats ++ "{\n" ++ ppTreesSparse ('\t':indent) cs ++ indent ++ "}" ++ ppCache cache

ppTreesSparse :: String -> HashMap Move (Tree A0.Statistics Move) -> String
ppTreesSparse indent ts = concat
	[ indent ++ ppMove move ++ "↦" ++ ppTreeSparse indent t ++ ",\n"
	| (move, t) <- HM.toList ts
	, A0.visitCount (statistics t) > 0
	]

ppTree :: String -> Tree A0.Statistics Move -> String
ppTree indent (Tree stats cs un cache) = ppStats stats ++ "{\n" ++ ppTrees indent' cs ++ indent' ++ ppStatss un ++ "\n" ++ indent ++ "}" ++ ppCache cache
	where indent' = '\t':indent

ppStats :: A0.Statistics -> String
ppStats (A0.Statistics visits prob val) = show (round (100*prob)) ++ "% " ++ show (fromIntegral (round (100*val))/100) ++ "/" ++ show (round visits)

ppTrees :: String -> HashMap Move (Tree A0.Statistics Move) -> String
ppTrees indent ts = concat
	[ indent ++ ppMove move ++ "↦" ++ ppTree indent t ++ ",\n"
	| (move, t) <- HM.toList ts
	]

ppStatss :: HashMap Move A0.Statistics -> String
ppStatss ss = concat
	[ ppMove move ++ "⇨" ++ ppStats stats ++ ", "
	| (move, stats) <- HM.toList ss
	]

ppCache :: Maybe A0.Statistics -> String
ppCache Nothing = ""
ppCache (Just stats) = "~" ++ show (fromIntegral (round (100*A0.cumulativeValuation stats))/100)

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
