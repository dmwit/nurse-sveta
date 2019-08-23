module Dr.Mario.Sveta where

import Data.IORef
import Data.List
import Data.Ord
import Data.Vector (Vector)
import Dr.Mario.Model
import Dr.Mario.Sveta.MCTS
import Dr.Mario.Sveta.Pathfinding
import Dr.Mario.Util
import System.Random.MWC
import System.Random.MWC.CondensedTable
import qualified Dr.Mario.Model as M
import qualified Dr.Mario.Sveta.Pathfinding as S
import qualified Data.Vector as V

data MCStats = MCStats
	{ visitCount :: !Double
	, cumulativeUtility :: !Double
	} deriving (Eq, Ord, Read, Show)

type MCScore = Down Double

data MCMove
	= AIMove Pill
	| ChanceMove Color Color
	deriving (Eq, Ord, Read, Show)

type MCM = IO

data MCPosition = MCPosition
	{ mboard :: IOBoard
	, auxState :: IORef AuxiliaryState
	, originalVirusCount :: Double
	}

data AuxiliaryState = AuxiliaryState
	{ pillsUsed :: !Double
	, pillsUsedSinceVirusClear :: !Double
	, lookahead :: Maybe (Color, Color)
	, virusesCleared :: !Int
	} deriving (Eq, Ord, Read, Show)

data MCPlayer = AI | Chance deriving (Bounded, Enum, Eq, Ord, Read, Show)

type DrMarioTree = MCTree MCStats MCScore MCMove
type DrMarioParameters = MCTSParameters MCM MCStats MCScore MCMove MCPosition MCPlayer

stallThreshold :: Double
stallThreshold = 20

mtoppedOut :: MCPosition -> IO Bool
mtoppedOut mcpos = check startingBottomLeftPosition `orM` check startingOtherPosition where
	check :: Position -> IO Bool
	check p = (Just Empty /=) <$> mget (mboard mcpos) p

timedOut :: AuxiliaryState -> Bool
timedOut aux = pillsUsedSinceVirusClear aux > stallThreshold

mtimedOut :: MCPosition -> IO Bool
mtimedOut mcpos = timedOut <$> readIORef (auxState mcpos)

mlost :: MCPosition -> IO Bool
mlost mcpos = orM (mtimedOut mcpos) (mtoppedOut mcpos)

won :: MCPosition -> AuxiliaryState -> Bool
won mcpos aux = originalVirusCount mcpos == fromIntegral (virusesCleared aux)

mwon :: MCPosition -> IO Bool
mwon mcpos = won mcpos <$> readIORef (auxState mcpos)

dmScore :: MCPlayer -> MCStats -> MCStats -> MCScore
dmScore AI statsParent statsCurrent = ucb1 (visitCount statsParent) (visitCount statsCurrent) (cumulativeUtility statsCurrent)
dmScore Chance _ statsCurrent = Down (visitCount statsCurrent)

dmEvaluate :: MCPosition -> MCM MCStats
dmEvaluate mcpos = do
	aux <- readIORef (auxState mcpos)
	longerIsBetter <- mtoppedOut mcpos
	let x &&& y | longerIsBetter = x+y
	            | otherwise      = x+1-y
	    pu = pillsUsed aux
	    clearUtility = fromIntegral (virusesCleared aux)
	    usageUtility = pu / (pu + 20)
	    maxUtility = originalVirusCount mcpos + 1
	pure MCStats
		{ visitCount = 1
		, cumulativeUtility = (clearUtility &&& usageUtility) / maxUtility
		}

allChanceMoves :: Vector Move
allChanceMoves = V.fromListN 6
	[ ChanceMove l r
	| l:rest <- tails [minBound .. maxBound]
	, r <- rest
	]

dmExpand :: MCPosition -> MCM (Vector MCMove)
dmExpand mcpos = do
	aux <- readIORef (auxState mcpos)
	done <- orM
		(return (timedOut aux || won mcpos aux))
		(mtoppedOut mcpos)
	if done then pure V.empty else case lookahead aux of
		-- TODO: return all pills, but share rollouts between neighboring
		-- flipped moves when they both exist
		--
		-- ...or maybe that would get too expensive, because then there's 2^n
		-- paths to update when we're processing an MCTree node n levels deep?
		-- but then what to do about moves that can't be rotated 180? hmmm...
		Nothing -> pure allChanceMoves
		Just (l, r) -> munsafeApproxReachable (mboard mcpos) (launchPill l r)

dmRoot :: Board -> MCM MCPosition
dmRoot b = do
	mb <- thaw b
	auxRef <- newIORef AuxiliaryState
		{ pillsUsed = 0
		, pillsUsedSinceVirusClear = 0
		, lookahead = Nothing
		, virusesCleared = 0
		}
	pure MCPosition
		{ mboard = mb
		, auxState = auxRef
		, originalVirusCount = fromIntegral viruses
		}
	where
	viruses = length
		[ ()
		| x <- [0..7]
		, y <- [0..15]
		, Just (Occupied _ Virus) <- [get b (Position x y)]
		]

dmTurn :: MCPosition -> MCM MCPlayer
dmTurn mcpos = maybe Chance (const AI) . lookahead <$> readIORef (auxState mcpos)

dmPlay :: MCPosition -> MCMove -> MCM ()
dmPlay mcpos (ChanceMove l r) = modifyIORef (auxState mcpos) (\aux -> aux { lookahead = Just (l, r) })
dmPlay mcpos (AIMove p) = do
	mVirusCount <- mplace (mboard mcpos) p
	case mVirusCount of
		Just virusCount -> modifyIORef (auxState mcpos) $ \aux -> aux
			{ pillsUsed = pillsUsed aux + 1
			, pillsUsedSinceVirusClear = if virusCount > 0 then 0 else pillsUsedSinceVirusClear aux + 1
			, lookahead = Nothing
			, virusesCleared = virusesCleared aux + virusCount
			}
		Nothing -> do
			b <- mfreeze (mboard mcpos)
			error
				$ "The impossible happened: the AI chose an illegal pill placement of "
				++ S.pp p ++ " on this board:\n"
				++ M.pp b

chanceMovesTable :: CondensedTableV MCMove
chanceMovesTable = tableFromProbabilities . V.fromListN 6 $
	[ (ChanceMove l r, if l == r then 1/9 else 2/9)
	| l:rest <- tails [minBound .. maxBound]
	, r <- rest
	]

dmSelect :: GenIO -> MCPosition -> Vector MCMove -> MCM MCMove
dmSelect gen _ ms = case V.head ms of
	ChanceMove _ _ -> genFromTable chanceMovesTable gen
	_ -> (ms `V.unsafeIndex`) <$> uniformR (0, V.length ms-1) gen

dmParameters :: GenIO -> Board -> DrMarioParameters
dmParameters gen b = MCTSParameters
	{ score = dmScore
	, evaluate = dmEvaluate
	, expand = dmExpand
	, root = dmRoot b
	, turn = dmTurn
	, play = dmPlay
	, select = dmSelect gen
	}

munsafeApproxReachable = undefined
