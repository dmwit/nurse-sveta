module Nurse.Sveta.Tomcats (
	DMParameters, dmParameters,
	GameStateSeed(..), initialTree,
	mcts, descend, unsafeDescend,
	evaluateFinalState, dumbEvaluation,
	winningValuation, losingValuation, clampCleared,
	dmFinished, dmPlay,
	SearchConfiguration(..), DMEvaluationProcedure,
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
import System.IO.Unsafe (unsafeInterleaveIO)
import Tomcats

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Tomcats.AlphaZero as A0

-- TODO:
-- 	* board/pill generation options like MWC vs LFSR, Algorithm X vs NES' algo,
-- 	  128 pre-determined pills vs on-the-fly, maybe SNES vs NES distro?
-- 	* neural net vs. not
-- 	* temperature/dirichlet params
data SearchConfiguration = SearchConfiguration
	{ c_puct :: Double
	, iterations :: Int
	} deriving (Eq, Ord, Read, Show)

type DMEvaluationProcedure = Procedure (GameState, Color, Color) (Double, HashMap PillContent (Vector (Vector Double)))

-- | The 'BoxMove' is ignored for 'Eq', 'Ord', and 'Hashable'.
data Move = RNG Color Color | Placement BoxMove Pill deriving (Show, Read)

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
	, originalVirusCount :: Int
	}

type DMParameters = Parameters IO Double A0.Statistics Move GameState

class GameStateSeed a where initialState :: a -> IO GameState

instance Gen a ~ GenIO => GameStateSeed (Gen a) where
	-- note to self: when the NES does it, it generates the pill sequence first,
	-- then the board
	initialState g = do
		level <- uniformRM (0, 20) g
		seed <- uniformRM (2, maxBound) g
		b <- mrandomBoard seed level
		vk <- newIORef 0
		pu <- newIORef 0
		fp <- newIORef 0
		pure GameState
			{ board = b
			, virusesKilled = vk
			, pillsUsed = pu
			, framesPassed = fp
			, originalVirusCount = 4*(level + 1)
			}

instance GameStateSeed Board where
	initialState b = pure GameState
		<*> thaw b
		<*> newIORef 0
		<*> newIORef 0
		<*> newIORef 0
		<*> pure (countViruses b)

initialTree :: GameStateSeed a => DMParameters -> a -> IO (GameState, Tree A0.Statistics Move)
initialTree params g = do
	s <- initialState g
	(_, t) <- Tomcats.initialize params s >>= preprocess params s
	pure (s, t)

dmParameters :: SearchConfiguration -> DMEvaluationProcedure -> DMParameters
dmParameters config eval = Parameters
	{ score = dmScore config
	, expand = dmExpand
	, clone = dmClone
	, play = dmPlay
	, preprocess = dmPreprocess eval
	}

dmScore :: SearchConfiguration -> Move -> A0.Statistics -> A0.Statistics -> Double
dmScore _ RNG{} _ stats = -A0.visitCount stats
dmScore config _ parent child = pucbA0 (c_puct config) (A0.priorProbability child) (A0.visitCount parent) (A0.visitCount child) (A0.cumulativeValuation child)

dmFinished :: GameState -> IO Bool
dmFinished gs = do
	cellL <- mget (board gs) startingBottomLeftPosition
	cellR <- mget (board gs) startingOtherPosition
	remaining <- (originalVirusCount gs-) <$> readIORef (virusesKilled gs)
	pure $ cellL /= Just Empty || cellR /= Just Empty || remaining <= 0

dmExpand :: GameState -> IO (A0.Statistics, HashMap Move A0.Statistics)
dmExpand gs = do
	finished <- dmFinished gs
	if finished
	then evaluateFinalState gs <&> \points -> (A0.Statistics 1 0 points, HM.empty)
	else pure rngExpansion -- this is a bit weird, but in order to share pathfinding, expansion actually happens in the preprocessor

-- score: 0 or  1/3 for finishing
--        up to 1/3 for each virus cleared; want the bot to learn early that clearing is good, so reward heavily for the first few viruses
--        up to 1/3 for clearing quickly if you win; the quicker you get, the harder it is to get quicker, so increase the reward more quickly when it's fast

-- | Assign a value to a winning board, given how many viruses the board
-- started with and how many frames it took. The frame count is clamped on your
-- behalf, so don't worry if it's negative or outrageously large or whatever.
winningValuation :: Int -> Double -> Double
winningValuation viruses frames_ = 1 - penalty / 3 where
	penalty = min 1 (frames / maxFrames)**0.5
	frames = max 1 frames_
	maxFrames = fromIntegral (shiftL viruses 9)

-- | Assign a value to a losing board, given how many viruses the board started
-- with and how many were cleared. You are responsible for clamping the clear
-- count if it might be out of range; see also 'clampCleared'.
losingValuation :: Int -> Double -> Double
losingValuation orig cleared = (cleared / fromIntegral orig)**0.5 / 3

-- | Given the original number of viruses on a board and a claimed number of
-- clears, clamp the claimed clears to be positive and at most the original
-- number. Useful as a preprocessing step before calling 'losingValuation'.
clampCleared :: Int -> Double -> Double
clampCleared orig = max 0 . min (fromIntegral orig)

evaluateFinalState :: GameState -> IO Double
evaluateFinalState gs = do
	clearedViruses <- readIORef (virusesKilled gs)
	frames <- readIORef (framesPassed gs)
	let origViruses = originalVirusCount gs
	pure $ if clearedViruses == origViruses
		then winningValuation origViruses (fromIntegral frames)
		else losingValuation  origViruses (fromIntegral clearedViruses)

rngExpansion :: (A0.Statistics, HashMap Move A0.Statistics)
rngExpansion = (mempty, HM.fromList [(RNG l r, A0.Statistics 0 (1/9) 0) | [l, r] <- replicateM 2 [minBound .. maxBound]])

dmClone :: GameState -> IO GameState
dmClone gs = pure GameState
	<*> cloneBoard (board gs)
	<*> cloneIORef (virusesKilled gs)
	<*> cloneIORef (pillsUsed gs)
	<*> cloneIORef (framesPassed gs)
	<*> pure (originalVirusCount gs)

cloneIORef :: IORef a -> IO (IORef a)
cloneIORef = readIORef >=> newIORef

cloneBoard :: IOBoard -> IO (IOBoard)
cloneBoard = mfreeze >=> thaw

dmPlay :: GameState -> Move -> IO ()
dmPlay gs = \case
	RNG l0 l1 -> pure ()
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
yCosts = V.fromList [undefined, undefined, 44, 42, 41, 41, 39, 39, 37, 37, 35, 35, 33, 34, 34, 34]

-- TODO: figure out the exact cost model for the NES, then move that into maryodel
approximateCostModel :: BoxMove -> Pill -> CleanupResults -> Int
approximateCostModel move pill counts = 0
	+ yCosts V.! y (bottomLeftPosition pill) -- throwing animation, lock animation, and pause between lock and next throw
	+ 1 -- rotate once
	+ max 0 (2*abs (xDelta move) - 1) -- move horizontally
	+ 1 - 2*yDelta move -- move vertically
	+ sum [16*n + 20 | n <- rowsFallen counts] -- fall time + clear animation

dmPreprocess :: DMEvaluationProcedure -> GameState -> Tree A0.Statistics Move -> IO (A0.Statistics, Tree A0.Statistics Move)
dmPreprocess eval gs t = if not (RNG Blue Blue `HM.member` unexplored t) then pure (mempty, t) else do
	moves <- munsafeApproxReachable (board gs) (launchPill Blue Red)
	-- doing fromListWith instead of fromList is probably a bit paranoid, but what the hell
	let symmetricMoves = HM.fromListWith smallerBox [(substPill Blue Blue p, m) | (p, m) <- HM.toList moves]
	children' <- flip HM.traverseWithKey (unexplored t) $ \(RNG l r) stats -> do
		future <- schedule eval (gs, l, r)
		~(valueEstimate, moveWeights) <- unsafeInterleaveIO future
		pure Tree
			{ statistics = A0.Statistics 1 (A0.priorProbability stats) valueEstimate
			, children = HM.empty
			, unexplored = A0.normalizeStatistics $ HM.fromList
				[ (Placement bm pill { content = pc' }, A0.Statistics 0 (moveWeights HM.! pc' V.! x V.! y) 0)
				| (pill, bm) <- HM.toList (if l == r then symmetricMoves else moves)
				, let Position x y = bottomLeftPosition pill
				      pc' = substPillContent l r (content pill)
				]
			, cachedEvaluation = Nothing
			}
	let childStats = (foldMap A0.statistics children') { A0.priorProbability = 0 }
	pure (childStats, t { statistics = statistics t <> childStats, children = children', unexplored = HM.empty })
	where
	substPill l r p = p { content = substPillContent l r (content p) }
	substPillContent l r pc = pc { bottomLeftColor = substColor l r (bottomLeftColor pc), otherColor = substColor l r (otherColor pc) }
	substColor l r = \case
		Blue -> l
		Red -> r
		Yellow -> error "The impossible happened in Dr.Mario.Tomcats.dmPreprocess: pathfinding on a blue-red pill resulted in placing a yellow pill half."

-- | Given the game state and the colors for the upcoming pill, guess where
-- moves will be made and how good the final outcome will be. The Vector's are
-- indexed by (x, y) position of the bottom left.
dumbEvaluation :: (GameState, Color, Color) -> IO (Double, HashMap PillContent (Vector (Vector Double)))
dumbEvaluation = \(s, l, r) -> do
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
