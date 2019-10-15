module Dr.Mario.Sveta where

import Control.Applicative
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.List
import Data.Vector (Vector)
import Dr.Mario.Model hiding (pp)
import Dr.Mario.Sveta.MCTS
import Dr.Mario.Sveta.Pathfinding
import Dr.Mario.Sveta.PP
import Dr.Mario.Util
import System.Random.MWC
import qualified Data.HashMap.Strict as HM
import qualified Dr.Mario.Model as M
import qualified Dr.Mario.Sveta.Pathfinding as S
import qualified Data.Vector as V

data MCStats = MCStats
	{ visitCount :: !Double
	, cumulativeUtility :: !Double
	} deriving (Eq, Ord, Read, Show)

meanUtility :: MCStats -> Double
meanUtility s = cumulativeUtility s / visitCount s

instance Monoid MCStats where mempty = MCStats 0 0
instance Semigroup MCStats where
	s1 <> s2 = MCStats
		{ visitCount = visitCount s1 + visitCount s2
		, cumulativeUtility = cumulativeUtility s1 + cumulativeUtility s2
		}

instance PP MCStats where
	pp (MCStats n q) = pp q ++ "/" ++ pp n ++ "=" ++ pp (q / n)

type MCScore = Double

data MCMove
	= AIMove Pill
	| ChanceMove Color Color
	deriving (Eq, Ord, Read, Show)

instance Hashable MCMove where
	hashWithSalt s (AIMove p) = s `hashWithSalt` (1 :: Int) `hashWithSalt` p
	hashWithSalt s (ChanceMove l r) = s `hashWithSalt` (2 :: Int) `hashWithSalt` l `hashWithSalt` r

instance PP MCMove where
	pp (AIMove p) = pp p
	pp (ChanceMove l r) = pp l ++ pp r

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

type DrMarioTree = MCTree MCStats MCMove
type DrMarioParameters = MCTSParameters MCM MCStats MCScore MCMove MCPosition

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

dmScore :: MCMove -> MCStats -> MCStats -> MCScore
dmScore (ChanceMove l r) _ statsCurrent = visitCount statsCurrent * if l == r then -2 else -1
dmScore _ statsParent statsCurrent = ucb1 (visitCount statsParent) (visitCount statsCurrent) (cumulativeUtility statsCurrent)

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

-- This is a slight lie, and intricately tied to how a couple other things
-- work, so, some commentary...
--
-- All combinations of left color and right color are actually possible in the
-- game. A pretty good mental model of how the chance moves works is that the
-- game chooses two colors uniformly and independently, then presents a pill
-- with those two colors.
--
-- However, strategically speaking, a pill and its flipped version almost
-- always have exactly the same meaning, since you almost certainly have time
-- to press the rotate button twice while maneuvering to where you want it to
-- go. So we normalize the pills in this vector in a way that identifies a pill
-- and its flipped version.
--
-- Note that there are 9 different "raw" pills, but only 6 different normalized
-- pills. This has some knock-on consequences: when we do our tree search, we
-- want to spend more time thinking about what to do with pills with two
-- different colors, since they're twice as likely to happen. So there are two
-- other parameters that have to be modified to understand the oddity we've
-- introduced by canonizing: 'dmSelect', which randomly chooses a move (we
-- return a vector of length 9 with only 6 distinct elements from
-- 'allChanceMoves', so that choosing uniformly from those 9 gets the right
-- distribution on the 6 possible outcomes), and 'dmScore', which informs the
-- search how much of its time to spend searching particular moves (we make
-- attempts to search pills with two different colors only count half as much,
-- so the search has to do them twice as often to get the same score).
allChanceMoves :: Vector MCMove
allChanceMoves = V.fromListN 9
	[ ChanceMove (min l r) (max l r)
	| l <- [minBound .. maxBound]
	, r <- [minBound .. maxBound]
	]

dmExpand :: MCPosition -> MCM (Vector MCMove)
dmExpand mcpos = do
	aux <- readIORef (auxState mcpos)
	done <- orM
		(return (timedOut aux || won mcpos aux))
		(mtoppedOut mcpos)
	if done then pure V.empty else case lookahead aux of
		Nothing -> pure allChanceMoves
		Just (l, r) -> V.fromList . map AIMove . toList
		           <$> munsafeApproxReachable (mboard mcpos) (launchPill l r)

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
				++ pp p ++ " on this board:\n"
				++ pp b

dmSelect :: GenIO -> MCPosition -> Vector MCMove -> MCM MCMove
dmSelect gen _ ms = (ms `V.unsafeIndex`) <$> uniformR (0, V.length ms-1) gen

accuratePathfindingThreshold :: Double
accuratePathfindingThreshold = 512

mapKey :: (Hashable k', Eq k') => (k -> k') -> HashMap k v -> HashMap k' v
mapKey f m = HM.fromList [(f k, v) | (k, v) <- HM.toList m]

dmPreprocess :: MCPosition -> DrMarioTree -> IO (MCStats, DrMarioTree)
dmPreprocess mcpos t
	| vc /= accuratePathfindingThreshold = nop
	| HM.null (children t) && null (unexplored t) = nop
	| otherwise = do
		aux <- readIORef (auxState mcpos)
		case lookahead aux of
			Nothing -> nop
			Just (l, r) -> do
				b <- mfreeze (mboard mcpos)
				-- TODO: track drop speed and parity
				-- TODO: create (and use) mreachable
				let placements = mapKey AIMove $ reachable b 13 (launchPill l r) Checking
				    children' = HM.intersection (children t) placements
				    unexplored' = HM.keys (HM.difference placements (children t))
				    stats' = foldMap statistics children'
				    dstats = MCStats
				    	{ visitCount = visitCount stats' - vc
				    	, cumulativeUtility = cumulativeUtility stats' - cu
				    	}
				pure (dstats, t { children = children', unexplored = unexplored' })
	where
	MCStats { visitCount = vc, cumulativeUtility = cu } = statistics t
	nop = pure (mempty, t)

dmParameters :: GenIO -> Board -> DrMarioParameters
dmParameters gen b = MCTSParameters
	{ score = dmScore
	, evaluate = dmEvaluate
	, expand = dmExpand
	, root = dmRoot b
	, play = dmPlay
	, select = dmSelect gen
	, preprocess = dmPreprocess
	}

-- | Produce a new set of parameters suitable for use with searching a
-- particular move's subtree. Resets the 'pillsUsedSinceVirusClear' to 0 and
-- deep-copies the board and auxiliary state.
dmReroot :: DrMarioParameters -> [MCMove] -> MCM DrMarioParameters
dmReroot params ms = do
	mcpos <- root params
	traverse_ (play params mcpos) ms
	b <- mfreeze (mboard mcpos)
	aux_ <- readIORef (auxState mcpos)
	let aux = aux_ { pillsUsedSinceVirusClear = 0 }
	pure params { root = liftA2
		(\mb auxRef -> mcpos { mboard = mb, auxState = auxRef })
		(thaw b)
		(newIORef aux)
		}
