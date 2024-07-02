module Nurse.Sveta.Tomcats (
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
	maximumOn,
	ppTreeIO, ppTreeSparseIO,
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
import Data.Monoid
import Data.Vector (Vector)
import Dr.Mario.Model
import Dr.Mario.Pathfinding
import Numeric
import Nurse.Sveta.STM.BatchProcessor
import Nurse.Sveta.Torch.Semantics
import System.Random.MWC
import System.Random.MWC.Distributions
import System.IO.Unsafe (unsafeInterleaveIO)
import Tomcats hiding (descend)
import Tomcats.AlphaZero.Float (Statistics(..))

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
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
	, temperature :: Double -- ^ 0 means always use the highest visitCount, large numbers trend toward the uniform distribution
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

initialTree :: GameStateSeed a => DMParameters -> a -> IO (GameState, Tree Statistics Move)
initialTree params g = do
	s <- initialState g
	(_, t) <- Tomcats.initialize params s >>= preprocess params s
	pure (s, t)

dmParameters :: SearchConfiguration -> Procedure NextNetInput NextNetOutput -> GenIO -> DMParameters
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

niEvaluateFinalState :: NextNetInput -> Float
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
	RNG l r -> writeIORef (lookbehind gs) (Lookahead l r)
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

dmPreprocess :: SearchConfiguration -> Procedure NextNetInput NextNetOutput -> GenIO -> GameState -> Tree Statistics Move -> IO (Statistics, Tree Statistics Move)
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
netInput :: GameState -> IO NextNetInput
netInput s = pure NextNetInput
	<*> mfreeze (board s)
	<*> readIORef (framesPassed s)
	<*> pure (originalVirusCount s)

dumbEvaluation :: NextNetInput -> IO NextNetOutput
dumbEvaluation = \ni -> pure NextNetOutput
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

-- very similar to A0.descend, except that:
-- * it pulls the temperature from the SearchConfiguration
-- * the meaning of the temperature is inverted (lower is more deterministic)
-- * it works with this module's Statistics rather than A0.Statistics
-- * it smoothly handles negative temperatures
-- * argument order is swapped for consistency with other stuff in nurse-sveta.hs
-- * some extra care is needed when choosing an unexplored move due to the oddities of our preprocessing step
descend :: GenIO -> SearchConfiguration -> DMParameters -> GameState -> Tree Statistics Move -> IO (Maybe (Move, Tree Statistics Move))
descend g config params s t = case (temperature config, weights) of
	(0, _) -> T.descend params visitCount s t
	(_, []) -> pure Nothing
	_ -> do
		i <- categorical (V.fromList weights) g
		let move = moves !! i
		play params s move
		case HM.lookup move (children t) of
			Just t' -> pure $ Just (move, t')
			Nothing -> Just . (,) move . snd <$> initialTree params s
	where
	tmpExp = recip (temperature config)
	-- the (1+) bit is to handle negative temperatures smoothly
	weight stats = realToFrac (1 + visitCount stats) ** tmpExp
	(moves, weights) = unzip . HM.toList $
		(weight . statistics <$> children t) `HM.union`
		(weight <$> unexplored t)

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
