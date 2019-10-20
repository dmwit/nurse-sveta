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
import qualified Data.HashSet as HS
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

negateStats :: MCStats -> MCStats
negateStats s = MCStats
	{ visitCount = -visitCount s
	, cumulativeUtility = -cumulativeUtility s
	}

(<>-) :: MCStats -> MCStats -> MCStats
s1 <>- s2 = s1 <> negateStats s2

type MCScore = Double

data ChanceMoveColorInterpretation = WithMirror | Positional deriving (Bounded, Enum, Eq, Ord, Read, Show)
instance Hashable ChanceMoveColorInterpretation where hashWithSalt s = hashWithSalt s . fromEnum

data ChanceMovePathfinding = Approximate | Exact deriving (Bounded, Enum, Eq, Ord, Read, Show)
instance Hashable ChanceMovePathfinding where hashWithSalt s = hashWithSalt s . fromEnum

instance PP ChanceMovePathfinding where
	pp Approximate = "|"
	pp Exact = "Z"

-- | If the interpretation is 'WithMirror', then the color fields are the
-- smallest color, then the largest; otherwise the color fields are the left
-- color, then the right.
data ChanceMove = Colors ChanceMoveColorInterpretation ChanceMovePathfinding Color Color
	deriving (Eq, Ord, Read, Show)

instance Hashable ChanceMove where
	hashWithSalt s (Colors i p c1 c2) = s `hashWithSalt` i `hashWithSalt` p `hashWithSalt` c1 `hashWithSalt` c2

instance PP ChanceMove where
	pp (Colors i p c1 c2) = ""
		++ pp c1 ++ pp c2
		++ (if i == WithMirror && c1 /= c2 then "/" ++ pp c2 ++ pp c1 else "")
		++ pp p

data MCMove
	= AIMove Pill
	| ChanceMove ChanceMove
	deriving (Eq, Ord, Read, Show)

instance Hashable MCMove where
	hashWithSalt s (AIMove p) = s `hashWithSalt` (1 :: Int) `hashWithSalt` p
	hashWithSalt s (ChanceMove m) = s `hashWithSalt` (2 :: Int) `hashWithSalt` m

instance PP MCMove where
	pp (AIMove p) = pp p
	pp (ChanceMove m) = pp m

type MCM = IO

data MCPosition = MCPosition
	{ mboard :: IOBoard
	, auxState :: IORef AuxiliaryState
	, originalVirusCount :: Double
	}

data AuxiliaryState = AuxiliaryState
	{ pillsUsed :: !Double
	, pillsUsedSinceVirusClear :: !Double
	, lookahead :: Maybe ChanceMove
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
dmScore (ChanceMove (Colors WithMirror _ l r)) _ statsCurrent | l /= r = visitCount statsCurrent / (-2)
dmScore (ChanceMove (Colors _ _ l r)) _ statsCurrent = -visitCount statsCurrent
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

-- The pills to consider when first looking at a position.
baseChanceMoves :: Vector MCMove
baseChanceMoves = V.fromListN 6
	[ ChanceMove (Colors WithMirror Approximate l r)
	| l <- [minBound .. maxBound]
	, r <- [l .. maxBound]
	]

-- Like baseChanceMoves, but arranged so that choosing uniformly at random from
-- this vector gets the right distribution of pills.
chanceMovesDistribution :: Vector MCMove
chanceMovesDistribution = V.fromListN 9
	[ ChanceMove (Colors WithMirror Approximate (min l r) (max l r))
	| l <- [minBound .. maxBound]
	, r <- [minBound .. maxBound]
	]

-- TODO: create (and use) a real mreachable
mreachable :: IOBoard -> Pill -> MCM (HashMap Pill [Move])
mreachable mb p = (\b -> reachable b 13 p Checking) <$> mfreeze mb

dmExpand :: MCPosition -> MCM (Vector MCMove)
dmExpand mcpos = do
	aux <- readIORef (auxState mcpos)
	done <- orM
		(return (timedOut aux || won mcpos aux))
		(mtoppedOut mcpos)
	if done then pure V.empty else case lookahead aux of
		Nothing -> pure baseChanceMoves
		Just (Colors _ Approximate l r)
			 -> V.fromList . map AIMove . toList
			<$> munsafeApproxReachable (mboard mcpos) (launchPill l r)
		Just (Colors _ Exact l r)
			 -> V.fromList . map AIMove . HM.keys
			<$> mreachable (mboard mcpos) (launchPill l r)

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
dmPlay mcpos (ChanceMove m) = modifyIORef (auxState mcpos) (\aux -> aux { lookahead = Just m })
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
dmSelect gen _ ms_ = (ms `V.unsafeIndex`) <$> uniformR (0, V.length ms-1) gen where
	ms = case V.head ms_ of
		ChanceMove (Colors WithMirror _ _ _) -> chanceMovesDistribution
		_ -> ms_

accuratePathfindingThreshold :: Double
accuratePathfindingThreshold = 512*9

-- TODO: change up some constants: make accuratePathFindingThreshold 512
-- instead of 512*9, and use 14 for fall time in mreachable instead of 13

mapKey :: (Hashable k', Eq k') => (k -> k') -> HashMap k v -> HashMap k' v
mapKey f m = HM.fromList [(f k, v) | (k, v) <- HM.toList m]

updateSubtree :: DrMarioTree -> HashMap MCMove ignored -> DrMarioTree
updateSubtree t ms = MCTree
	{ statistics = statistics t <> foldMap statistics children' <>- foldMap statistics (children t)
	, children = children'
	, unexplored = unexplored'
	} where
	children' = HM.intersection (children t) ms
	unexplored' = HM.keys (HM.difference ms (children t))

dmPreprocess :: MCPosition -> DrMarioTree -> IO (MCStats, DrMarioTree)
dmPreprocess mcpos t
	| visitCount (statistics t) /= accuratePathfindingThreshold = nop
	| HM.null (children t) && null (unexplored t) = nop
	| otherwise = do
		aux <- readIORef (auxState mcpos)
		case lookahead aux of
			Nothing -> do
				placementsM <- mreachable (mboard mcpos) (launchPill Red Blue)
				-- TODO: track drop speed and parity
				let placementsS = HM.keysSet placementsM
				    onOrderings :: (Color -> Color -> a) -> Color -> Color -> [a]
				    (i, onOrderings) = if placementsS == HS.map aboutFacePill placementsS
				    	then (WithMirror, \f l r -> [f (min l r) (max l r)])
				    	else (Positional, \f l r -> [f l r, f r l])
				    moves l r = mapKey (AIMove . adjustPill l r) placementsM
				    children' = HM.fromList
				    	[ (ChanceMove m, updateSubtree t (moves c1 c2))
				    	| (ChanceMove (Colors _ _ l r), t) <- HM.toList (children t)
				    	, (m, c1, c2) <- onOrderings (\c1 c2 -> (Colors i Exact c1 c2, c1, c2)) l r
				    	]
				    unexplored' =
				    	[ ChanceMove m
				    	| ChanceMove (Colors _ _ l r) <- unexplored t
				    	, m <- onOrderings (Colors i Exact) l r
				    	]
				    dstats = foldMap statistics children' <>- foldMap statistics (children t)
				pure (dstats, t
					{ children = children'
					, unexplored = unexplored'
					})
			Just _ -> nop
	where
	nop = pure (mempty, t)

	adjustColor l r Red = l
	adjustColor l r Blue = r
	adjustColor l r Yellow = error "The impossible happened: saw Yellow in adjustColor."
	adjustPillContent l r pc = pc
		{ bottomLeftColor = adjustColor l r (bottomLeftColor pc)
		, otherColor = adjustColor l r (otherColor pc)
		}
	adjustPill l r p = p { content = adjustPillContent l r (content p) }

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
