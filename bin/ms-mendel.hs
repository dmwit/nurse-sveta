module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Aeson
import Data.Bits
import Data.Char
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.List
import Data.Ord
import Data.Traversable
import Data.Vector (Vector)
import Dr.Mario.Model
import Dr.Mario.Pathfinding
import GHC.Generics
import GI.Gtk
import Nurse.Sveta.Files
import Nurse.Sveta.Genome
import Nurse.Sveta.STM
import Nurse.Sveta.Tomcats
import Nurse.Sveta.Util
import Nurse.Sveta.Widget
import System.Environment
import System.Random.MWC
import System.Random.MWC.Distributions
import System.Mem
import Util

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

-- ╭╴w╶────────────────────────╮
-- │╭╴top╶────────────────────╮│
-- ││╭╴play1╶╮╭╴play2╶╮╭╴gen╶╮││
-- ││╰───────╯╰───────╯╰─────╯││
-- │╰─────────────────────────╯│
-- ╰───────────────────────────╯
main :: IO ()
main = do
	torchPlusGtkFix
	dir <- getXdgDirectory XdgConfig "ms-mendel"
	mmc <- eitherDecodeFileStrict (dir </> "config.json") >>= either fail pure
	app <- new Application []
	on app #activate do
		forceQuitRef <- newIORef False
		jobs <- newEmptyMVar
		top <- new Box [#orientation := OrientationHorizontal, #spacing := 10]
		play1 <- newThreadManager "evaluation" Green (evaluationThreadView mmc jobs)
		play2 <- newThreadManager "evaluation" Green (evaluationThreadView mmc jobs)
		gen <- newThreadManager "evolution" Green (evolutionThreadView mmc jobs)
		#append top =<< tmWidget play1
		#append top =<< tmWidget play2
		#append top =<< tmWidget gen
		let ethreads = mmcInitialEvaluationThreads mmc
		replicateM_ ((ethreads+1) `quot` 2) (tmStartThread play1)
		replicateM_ ( ethreads    `quot` 2) (tmStartThread play2)
		replicateM_ (mmcInitialEvolutionThreads mmc) (tmStartThread gen)

		w <- new Window $ tail [undefined
			, #title := "Ms. Mendel"
			, #application := app
			, #child := top
			, #defaultWidth := 1500
			, #defaultHeight := 1000
			, On #closeRequest $ readIORef forceQuitRef >>= \case
				True -> pure False
				False -> do
					writeIORef forceQuitRef True
					forkIO do
						threadDelay 1000000
						forever (putMVar jobs undefined) -- unblock evaluation threads waiting for a job
					forkIO do
						threadDelay 1000000
						forever (takeMVar jobs) -- unblock dying threads trying to push jobs into the queue
					True <$ play1 `tmDieThen` play2 `tmDieThen` (performGC >> #quit app)
			]

		#show w
	args <- getArgs
	() <$ #run app (Just args)

data MsMendelConfig = MsMendelConfig
	{ mmcInitialEvaluationThreads :: Int
	, mmcInitialEvolutionThreads :: Int
	, mmcInitialPopulation :: Int
	, mmcPatternWidth, mmcPatternHeight :: Int
	, mmcInitialPatterns :: Int
	, mmcInitialPatternThreshold :: Float
	, mmcPillCycleLength :: Int
	, mmcEvaluationRateLimit :: Int
	, mmcSurvivors :: Int
	, mmcBreeders :: Int
	, mmcMutators :: Int
	, mmcOffspring :: Int
	, mmcGeneInsertions :: Int
	, mmcGeneDeletions :: Int
	, mmcPatternToggles :: Int
	, mmcScoreToggles :: Int
	, mmcScoreAdjustments :: Int
	, mmcMaxScoreAdjustmentFactor :: Float
	, mmcMaxLevel :: Int
	} deriving (Eq, Ord, Read, Show, Generic)

instance FromJSON MsMendelConfig where
	parseJSON = genericParseJSON defaultOptions
		{ fieldLabelModifier = \case
			'm':'m':'c':s -> drop 1 [c' | c <- s, c' <- ['-' | isUpper c] ++ [toLower c]]
			other -> error $ "unexpected field name " ++ show other ++ " in parseJSON @MsMendelConfig"
		, allowOmittedFields = False
		, rejectUnknownFields = True
		}

data Job = Job
	{ jGenome :: Genome
	, jGame :: GameState
	, jLookaheads :: [Lookahead]
	, jID :: Int
	, jReply :: MVar Evaluation
	}

data Evaluation = Evaluation
	{ eID :: Int
	, eViruses :: Int
	, eFramesToLastKill :: Int
	} deriving (Eq, Ord, Read, Show)

evaluationThreadView :: MsMendelConfig -> MVar Job -> IO ThreadView
evaluationThreadView mmc jobs = do
	let blank = PSM
	    	{ psmBoard = emptyBoard 8 16
	    	, psmLookahead = Nothing
	    	, psmOverlay = []
	    	}
	psv <- newPlayerStateView blank
	w <- psvWidget psv
	psmRef <- newTVarIO blank
	tvNew w (readTVarIO psmRef >>= psvSet psv) (evaluationThread mmc jobs psmRef)

evaluationThread :: MsMendelConfig -> MVar Job -> TVar PlayerStateModel -> StatusCheck -> IO ()
evaluationThread mmc jobs psmRef sc = createSystemRandom >>= \rng -> forever do
	scIO_ sc
	job <- takeMVar jobs
	-- we need to make a copy so that scIO below can put the original game back
	-- into the queue
	gs <- cloneGameState (jGame job)
	let moveLoop frames [] = moveLoop frames (jLookaheads job)
	    moveLoop frames (lk:lks) = finished gs >>= \b -> if b then pure frames else do
	    	scIO sc (putMVar jobs job)
	    	cur <- mfreeze (board gs)
	    	atomically $ writeTVar psmRef PSM { psmBoard = cur, psmLookahead = Just lk, psmOverlay = [] }
	    	fp <- readIORef (framesPassed gs)
	    	pu <- readIORef (pillsUsed gs)
	    	vk <- readIORef (virusesKilled gs)
	    	placements <- mapproxReachable (board gs) (fp .&. 1 /= fromEnum (originalSensitive gs)) (gravity (speed gs) pu)
	    	let moves = V.fromList . HM.toList . HM.fromListWith shorterPath $
	    	    	[(mpPill placement lk, path) | (placement, path) <- HM.toList placements]
	    	next <- for moves \(pill, path) -> do
	    		gs' <- cloneGameState gs
	    		playMove gs' path pill
	    		mfreeze (board gs')
	    	let scores = gEvaluate (jGenome job) cur next
	    	    bestScore = V.maximum scores
	    	    bestIndices = V.findIndices (bestScore==) scores
	    	    rateLimit = mmcEvaluationRateLimit mmc
	    	(pill, path) <- (moves V.!) <$> uniformV' rng bestIndices
	    	playMove gs path pill
	    	vk' <- readIORef (virusesKilled gs)
	    	frames' <- if vk' > vk then readIORef (framesPassed gs) else pure frames
	    	when (rateLimit > 0) (threadDelay rateLimit)
	    	moveLoop frames' lks
	frames <- moveLoop 0 []
	vk <- readIORef (virusesKilled gs)
	putMVar (jReply job) Evaluation
		{ eID = jID job
		, eViruses = vk
		, eFramesToLastKill = frames
		}

data GenerationOverview = GenerationOverview
	{ goID :: Int
	, goPopulationSize :: Int
	, goPopulationEvaluated :: Int
	, goBestSoFar :: Maybe Evaluation
	, goWorstSoFar :: Maybe Evaluation
	, goMinSize, goFirstQuartileSize, goMedianSize, goLastQuartileSize, goMaxSize :: Double
	} deriving (Eq, Ord, Read, Show)

evolutionThreadView :: MsMendelConfig -> MVar Job -> IO ThreadView
evolutionThreadView mmc jobs = do
	pop <- V.replicateM (mmcInitialPopulation mmc) $ newGenome (mmcPatternWidth mmc) (mmcPatternHeight mmc) (mmcInitialPatterns mmc) (mmcInitialPatternThreshold mmc)
	replies <- newEmptyMVar
	rng <- createSystemRandom
	overviewRef <- newTVarIO GenerationOverview
		{ goID = 0
		, goPopulationSize = V.length pop
		, goPopulationEvaluated = 0
		, goBestSoFar = Nothing
		, goWorstSoFar = Nothing
		, goMinSize = 0
		, goFirstQuartileSize = 0
		, goMedianSize = 0
		, goLastQuartileSize = 0
		, goMaxSize = 0
		}

	genDesc <- new Label [#label := "generation", #halign := AlignStart]
	popDesc <- new Label [#label := "population size", #halign := AlignStart]
	evalDesc <- new Label [#label := "evaluated population", #halign := AlignStart]
	bestVirDesc <- new Label [#label := "most viruses killed this generation", #halign := AlignStart]
	bestFrameDesc <- new Label [#label := "\tframes required", #halign := AlignStart]
	wrstVirDesc <- new Label [#label := "least viruses killed this generation", #halign := AlignStart]
	wrstFrameDesc <- new Label [#label := "\tframes required", #halign := AlignStart]
	pctDesc <- new Label [#label := "genome size by percentile", #halign := AlignStart]
	pct0Desc <- new Label [#label := "\tmin", #halign := AlignStart]
	pct25Desc <- new Label [#label := "\t25%", #halign := AlignStart]
	pct50Desc <- new Label [#label := "\t50%", #halign := AlignStart]
	pct75Desc <- new Label [#label := "\t75%", #halign := AlignStart]
	pct99Desc <- new Label [#label := "\tmax", #halign := AlignStart]
	genVal <- new Label [#halign := AlignEnd]
	popVal <- new Label [#halign := AlignEnd]
	evalVal <- new Label [#halign := AlignEnd]
	bestVirVal <- new Label [#halign := AlignEnd]
	bestFrameVal <- new Label [#halign := AlignEnd]
	wrstVirVal <- new Label [#halign := AlignEnd]
	wrstFrameVal <- new Label [#halign := AlignEnd]
	pctVal <- new Label [#halign := AlignEnd]
	pct0Val <- new Label [#halign := AlignEnd]
	pct25Val <- new Label [#halign := AlignEnd]
	pct50Val <- new Label [#halign := AlignEnd]
	pct75Val <- new Label [#halign := AlignEnd]
	pct99Val <- new Label [#halign := AlignEnd]

	let refresh = do
	    	overview <- readTVarIO overviewRef
	    	set genVal [#label := tshow (goID overview)]
	    	set popVal [#label := tshow (goPopulationSize overview)]
	    	set evalVal [#label := tshow (goPopulationEvaluated overview)]
	    	case goBestSoFar overview of
	    		Nothing -> set bestVirVal [#label := ""] >> set bestFrameVal [#label := ""]
	    		Just e -> set bestVirVal [#label := tshow (eViruses e)] >> set bestFrameVal [#label := tshow (eFramesToLastKill e)]
	    	case goWorstSoFar overview of
	    		Nothing -> set wrstVirVal [#label := ""] >> set wrstFrameVal [#label := ""]
	    		Just e -> set wrstVirVal [#label := tshow (eViruses e)] >> set wrstFrameVal [#label := tshow (eFramesToLastKill e)]
	    	set pct0Val [#label := tshow (goMinSize overview)]
	    	set pct25Val [#label := tshow (goFirstQuartileSize overview)]
	    	set pct50Val [#label := tshow (goMedianSize overview)]
	    	set pct75Val [#label := tshow (goLastQuartileSize overview)]
	    	set pct99Val [#label := tshow (goMaxSize overview)]

	table <- new Grid []
	rowRef <- newIORef 0
	let attachPair a b = do
	    	row <- readIORef rowRef
	    	#attach table a 0 row 1 1
	    	#attach table b 1 row 1 1
	    	writeIORef rowRef (row+1)
	attachPair genDesc genVal
	attachPair popDesc popVal
	attachPair evalDesc evalVal
	attachPair bestVirDesc bestVirVal
	attachPair bestFrameDesc bestFrameVal
	attachPair wrstVirDesc wrstVirVal
	attachPair wrstFrameDesc wrstFrameVal
	attachPair pctDesc pctVal
	attachPair pct0Desc pct0Val
	attachPair pct25Desc pct25Val
	attachPair pct50Desc pct50Val
	attachPair pct75Desc pct75Val
	attachPair pct99Desc pct99Val

	tvNew table refresh (evolutionThread mmc jobs replies overviewRef rng pop)

evolutionThread :: MsMendelConfig -> MVar Job -> MVar Evaluation -> TVar GenerationOverview -> GenIO -> Vector Genome -> StatusCheck -> IO ()
evolutionThread mmc jobs replies overviewRef rng pop0 sc = go pop0 where
	go pop = do
		scIO_ sc
		gs0 <- initialState (rng, mmcMaxLevel mmc)
		lks <- replicateM (mmcPillCycleLength mmc) (sampleRNG' rng)
		tid <- forkIO $ V.iforM_ pop \i genome -> do
			gs <- cloneGameState gs0
			putMVar jobs Job
				{ jGenome = genome
				, jGame = gs
				, jLookaheads = lks
				, jID = i
				, jReply = replies
				}
		evals_ <- replicateM (V.length pop) do
			scIO sc do
				forkIO . forever $ takeMVar replies
				killThread tid
			v <- takeMVar replies
			atomically $ v <$ modifyTVar overviewRef \overview -> overview
				{ goPopulationEvaluated = goPopulationEvaluated overview + 1
				, goBestSoFar = Just $ maybe v (minOn (what'sBad pop) v) (goBestSoFar overview)
				, goWorstSoFar = Just $ maybe v (maxOn (what'sBad pop) v) (goWorstSoFar overview)
				}
		let evals = sortOn (what'sBad pop) evals_
		    sortedPop = V.fromList evals <&> \e -> pop V.! eID e
		    survivors = V.take (mmcSurvivors mmc) sortedPop
		offspring <- breed mmc rng (V.take (mmcBreeders mmc) sortedPop)
		mutations <- mutate mmc rng (V.take (mmcMutators mmc) sortedPop)
		let pop' = survivors <> offspring <> mutations
		    sizes = sort . V.toList $ gSize <$> pop'
		    quartile n = case (V.length pop' * n) `quotRem` 4 of
		    	(q, r) -> fromIntegral (sizes !!  q   ) * (fromIntegral (4-r) / 4)
		    	        + fromIntegral (sizes !! (q+1)) * (fromIntegral    r  / 4)
		atomically $ modifyTVar overviewRef \overview -> overview
			{ goID = goID overview + 1
			, goPopulationEvaluated = 0
			, goPopulationSize = V.length pop'
			, goBestSoFar = Nothing
			, goWorstSoFar = Nothing
			, goMinSize = fromIntegral $ head sizes
			, goMaxSize = fromIntegral $ last sizes
			, goFirstQuartileSize = quartile 1
			, goMedianSize = quartile 2
			, goLastQuartileSize = quartile 3
			}
		go pop'

what'sBad :: Vector Genome -> Evaluation -> (Int, Int, Int)
what'sBad pop e = (-eViruses e, eFramesToLastKill e, -gSize (pop V.! eID e))

breed :: MsMendelConfig -> GenIO -> Vector Genome -> IO (Vector Genome)
breed mmc rng pop
	| V.length pop < 2 = pure V.empty
	| otherwise = V.replicateM (mmcOffspring mmc) do
		(g, g') <- chooseTwo
		shuffledIndices <- uniformShuffle ((Left <$> V.generate (gSize g) id) <> (Right <$> V.generate (gSize g') id)) rng
		len <- (1+) <$> uniformVI' rng shuffledIndices
		let (indices, indices') = V.partitionWith id (V.take len shuffledIndices)
		liftJ2 gAppend (gIndices g (V.toList indices)) (gIndices g' (V.toList indices'))
	where
	chooseTwo = do
		[a, b] <- replicateM 2 (uniformVI' rng pop)
		if a == b then chooseTwo else pure (pop V.! a, pop V.! b)

mutate :: MsMendelConfig -> GenIO -> Vector Genome -> IO (Vector Genome)
mutate mmc rng pop = do
	ins <- V.replicateM (mmcGeneInsertions mmc) insertGene
	del <- V.replicateM (mmcGeneDeletions mmc) deleteGene
	pat <- V.replicateM (mmcPatternToggles mmc) togglePattern
	sco <- V.replicateM (mmcScoreToggles mmc) toggleScore
	adj <- V.replicateM (mmcScoreAdjustments mmc) adjustScore
	pure $ mconcat [ins, del, pat, sco, adj]
	where
	insertGene = liftJ2 gAppend
		(uniformV' rng pop)
		(newGenome (mmcPatternWidth mmc) (mmcPatternHeight mmc) 1 (mmcInitialPatternThreshold mmc))
	deleteGene = do
		g <- uniformV' rng pop
		let sz = gSize g
		if sz <= 1 then deleteGene else do
			n <- uniformRM (0, sz - 1) rng
			gIndices g $ [0..n-1] ++ [n+1..sz-1]
	togglePattern = do
		g <- uniformV' rng pop >>= gClone
		pat <- uniformRM (0, gSize g - 1) rng
		chan <- uniformV' rng allChannels
		x <- uniformRM (0, mmcPatternWidth mmc - 1) rng
		y <- uniformRM (0, mmcPatternHeight mmc - 1) rng
		case chan of
			Left  color -> gSetColorPattern g pat color x y . not $ gGetColorPattern g pat color x y
			Right shape -> gSetShapePattern g pat shape x y . not $ gGetShapePattern g pat shape x y
		pure g
	toggleScore = do
		g <- uniformV' rng pop >>= gClone
		pat <- uniformRM (0, gSize g - 1) rng
		gSetPatternScore g pat . negate $ gGetPatternScore g pat
		pure g
	adjustScore = do
		g <- uniformV' rng pop >>= gClone
		pat <- uniformRM (0, gSize g - 1) rng
		let range = log (mmcMaxScoreAdjustmentFactor mmc)
		factor <- exp <$> uniformRM (-range, range) rng
		gSetPatternScore g pat . (factor*) $ gGetPatternScore g pat
		pure g

allChannels :: Vector (Either (WithSentinels Color) (WithSentinels Shape))
allChannels = fmap Left allColorSentinels <> fmap Right allShapeSentinels

allColorSentinels :: Vector (WithSentinels Color)
allColorSentinels = addSentinels [minBound..maxBound]

allShapeSentinels :: Vector (WithSentinels Shape)
allShapeSentinels = addSentinels [Virus, Disconnected, East, West] -- North, South = Disconnected

addSentinels :: [a] -> Vector (WithSentinels a)
addSentinels as = V.fromList $ map NonSentinel as ++ [EmptySentinel, OutOfBoundsSentinel]
