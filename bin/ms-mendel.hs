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
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Vector.Mutable as VM

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
	, mmcPillCycleLength :: Int
	, mmcEvaluationRateLimit :: Int
	, mmcSurvivors :: Int
	, mmcBreeders :: Int
	, mmcMutators :: Int
	, mmcOffspring :: Int
	, mmcGeneReplacements :: Int
	, mmcGeneDeletions :: Int
	, mmcPatternToggles :: Int
	, mmcScoreToggles :: Int
	, mmcScoreAdjustments :: Int
	, mmcMaxScoreAdjustmentFactor :: Float
	, mmcMaxLevel :: Int
	, mmcMaxGenomeSize :: Int
	, mmcMaxPillsPerKill :: Int
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

instance Semigroup Evaluation where
	e <> e' = e
		{ eViruses = eViruses e + eViruses e'
		, eFramesToLastKill = eFramesToLastKill e + eFramesToLastKill e'
		}

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
	let moveLoop pills frames [] = moveLoop pills frames (jLookaheads job)
	    moveLoop pills frames (lk:lks) = stopMoving pills gs >>= \b -> if b then pure frames else do
	    	scIO sc (putMVar jobs job)
	    	cur <- mfreeze (board gs)
	    	-- TODO: would be nice to do this after playing the move, since we
	    	-- only really ever see this in detail when evaluation threads have
	    	-- finished a generation's games and are waiting for their peers,
	    	-- but getting the lookahead right is obnoxious
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
	    	(pills', frames') <- if vk' > vk
	    		then liftM2 (,) (readIORef (pillsUsed gs)) (readIORef (framesPassed gs))
	    		else pure (pills, frames)
	    	when (rateLimit > 0) (threadDelay rateLimit)
	    	moveLoop pills' frames' lks
	frames <- moveLoop 0 0 []
	vk <- readIORef (virusesKilled gs)
	putMVar (jReply job) Evaluation
		{ eID = jID job
		, eViruses = vk
		, eFramesToLastKill = frames
		}
	where
	stopMoving pills gs = finished gs <||> do
		pills' <- readIORef (pillsUsed gs)
		pure (pills' > pills + mmcMaxPillsPerKill mmc)

data GenerationOverview = GenerationOverview
	{ goID :: Int
	, goPopulationSize :: Int
	, goLevelsPlayed :: Int
	, goLevelsToPlay :: Int
	, goBestSoFar :: Maybe Evaluation
	, goWorstSoFar :: Maybe Evaluation
	, goMinSize, goFirstQuartileSize, goMedianSize, goLastQuartileSize, goMaxSize :: Double
	} deriving (Eq, Ord, Read, Show)

-- use the Jeffreys prior for Bernoulli distributions, β(½,½), to choose the
-- Bernoulli parameter
newJeffreysGenome :: GenIO -> Int -> Int -> Int -> IO Genome
newJeffreysGenome rng w h n = newGenome w h n . realToFrac =<< beta 0.5 0.5 rng

evolutionThreadView :: MsMendelConfig -> MVar Job -> IO ThreadView
evolutionThreadView mmc jobs = do
	rng <- createSystemRandom
	pop <- V.replicateM (mmcInitialPopulation mmc) $ newJeffreysGenome rng (mmcPatternWidth mmc) (mmcPatternHeight mmc) (mmcInitialPatterns mmc)
	replies <- newEmptyMVar
	overviewRef <- newTVarIO GenerationOverview
		{ goID = 0
		, goPopulationSize = V.length pop
		, goLevelsPlayed = 0
		, goLevelsToPlay = V.length pop * (mmcMaxLevel mmc + 1)
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
	totDesc <- new Label [#label := "boards to evaluate", #halign := AlignStart]
	evalDesc <- new Label [#label := "boards evaluated", #halign := AlignStart]
	levDesc <- new Label [#label := "currently on level", #halign := AlignStart]
	posVirDesc <- new Label [#label := "viruses available to kill", #halign := AlignStart]
	bestVirDesc <- new Label [#label := "most viruses killed this generation", #halign := AlignStart]
	bestFrameDesc <- new Label [#label := "\tframes required", #halign := AlignStart]
	bestMinDesc <- new Label [#label := "\tminutes required", #halign := AlignStart]
	wrstVirDesc <- new Label [#label := "least viruses killed this generation", #halign := AlignStart]
	wrstFrameDesc <- new Label [#label := "\tframes required", #halign := AlignStart]
	wrstMinDesc <- new Label [#label := "\tminutes required", #halign := AlignStart]
	pctDesc <- new Label [#label := "genome size by percentile", #halign := AlignStart]
	pct0Desc <- new Label [#label := "\tmin", #halign := AlignStart]
	pct25Desc <- new Label [#label := "\t25%", #halign := AlignStart]
	pct50Desc <- new Label [#label := "\t50%", #halign := AlignStart]
	pct75Desc <- new Label [#label := "\t75%", #halign := AlignStart]
	pct99Desc <- new Label [#label := "\tmax", #halign := AlignStart]
	genVal <- new Label [#halign := AlignEnd]
	popVal <- new Label [#halign := AlignEnd]
	totVal <- new Label [#halign := AlignEnd]
	evalVal <- new Label [#halign := AlignEnd]
	levVal <- new Label [#halign := AlignEnd]
	posVirVal <- new Label [#halign := AlignEnd]
	bestVirVal <- new Label [#halign := AlignEnd]
	bestFrameVal <- new Label [#halign := AlignEnd]
	bestMinVal <- new Label [#halign := AlignEnd]
	wrstVirVal <- new Label [#halign := AlignEnd]
	wrstFrameVal <- new Label [#halign := AlignEnd]
	wrstMinVal <- new Label [#halign := AlignEnd]
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
	    	set totVal [#label := tshow (goLevelsToPlay overview)]
	    	set evalVal [#label := tshow (goLevelsPlayed overview)]
	    	let lev = goLevelsPlayed overview `div` goPopulationSize overview
	    	    vir = 2 * (lev+1) * (lev+2)
	    	set levVal [#label := tshow lev]
	    	set posVirVal [#label := tshow vir]
	    	case goBestSoFar overview of
	    		Nothing -> set bestVirVal [#label := ""] >> set bestFrameVal [#label := ""] >> set bestMinVal [#label := ""]
	    		Just e -> do
	    			set bestVirVal [#label := tshow (eViruses e)]
	    			set bestFrameVal [#label := tshow (eFramesToLastKill e)]
	    			set bestMinVal [#label := asMinutes (eFramesToLastKill e)]
	    	case goWorstSoFar overview of
	    		Nothing -> set wrstVirVal [#label := ""] >> set wrstFrameVal [#label := ""] >> set wrstMinVal [#label := ""]
	    		Just e -> do
	    			set wrstVirVal [#label := tshow (eViruses e)]
	    			set wrstFrameVal [#label := tshow (eFramesToLastKill e)]
	    			set wrstMinVal [#label := asMinutes (eFramesToLastKill e)]
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
	attachPair totDesc totVal
	attachPair evalDesc evalVal
	attachPair levDesc levVal
	attachPair posVirDesc posVirVal
	attachPair bestVirDesc bestVirVal
	attachPair bestFrameDesc bestFrameVal
	attachPair bestMinDesc bestMinVal
	attachPair wrstVirDesc wrstVirVal
	attachPair wrstFrameDesc wrstFrameVal
	attachPair wrstMinDesc wrstMinVal
	attachPair pctDesc pctVal
	attachPair pct0Desc pct0Val
	attachPair pct25Desc pct25Val
	attachPair pct50Desc pct50Val
	attachPair pct75Desc pct75Val
	attachPair pct99Desc pct99Val

	tvNew table refresh (evolutionThread mmc jobs replies overviewRef rng pop)

asMinutes :: Int -> T.Text
asMinutes frames = tshow wholeMinutes <> ":" <> zeroPad 2 (tshow wholeSeconds) <> "." <> zeroPad 3 (tshow wholeMillis) where
	minutes = fromIntegral frames / ntscFrameRate / 60
	wholeMinutes = floor minutes
	seconds = 60 * (minutes - fromIntegral wholeMinutes)
	wholeSeconds = floor seconds
	millis = 1000 * (seconds - fromIntegral wholeSeconds)
	wholeMillis = round millis
	zeroPad n t = T.replicate (n - T.length t) "0" <> t

evolutionThread :: MsMendelConfig -> MVar Job -> MVar Evaluation -> TVar GenerationOverview -> GenIO -> Vector Genome -> StatusCheck -> IO ()
evolutionThread mmc jobs replies overviewRef rng pop0 sc = go pop0 where
	go pop = do
		scIO_ sc
		gslks <- forM [0..mmcMaxLevel mmc] \lev -> do
			gs <- initialState (ExactLevel rng lev)
			lk <- replicateM (mmcPillCycleLength mmc) (sampleRNG' rng)
			pure (gs, lk)
		tid <- forkIO $ forM_ gslks \(gs0, lks) -> V.iforM_ pop \i genome -> do
			gs <- cloneGameState gs0
			putMVar jobs Job
				{ jGenome = genome
				, jGame = gs
				, jLookaheads = lks
				, jID = i
				, jReply = replies
				}
		evals <- VM.generate (V.length pop) \i -> Evaluation { eID = i, eViruses = 0, eFramesToLastKill = 0 }
		forM_ gslks \_ -> do
			atomically $ modifyTVar overviewRef \overview -> overview { goWorstSoFar = Nothing }
			forM_ pop \_ -> do
				scIO sc do
					forkIO . forever $ takeMVar replies
					killThread tid
				deval <- takeMVar replies
				VM.modify evals (deval<>) (eID deval)
				eval <- VM.read evals (eID deval)
				atomically $ modifyTVar overviewRef \overview -> overview
					{ goLevelsPlayed = goLevelsPlayed overview + 1
					, goBestSoFar = Just $ maybe eval (minOn (what'sBad pop) eval) (goBestSoFar overview)
					, goWorstSoFar = Just $ maybe eval (maxOn (what'sBad pop) eval) (goWorstSoFar overview)
					}
		V.sortBy (comparing (what'sBad pop)) evals
		frozenEvals <- V.freeze evals
		let sortedIDs = eID <$> frozenEvals
		    sortedPop = V.backpermute pop sortedIDs
		    survivors = V.take (mmcSurvivors mmc) sortedPop
		    report nm val = putStrLn (nm ++ ": " ++ show val)

		report "sortedIDs" sortedIDs
		report "eViruses" (eViruses <$> frozenEvals)
		report "eFramesToLastKill" (eFramesToLastKill <$> frozenEvals)
		report "sizes" (gSize <$> sortedPop)
		putStrLn ""

		offspring <- breed mmc rng (V.take (mmcBreeders mmc) sortedPop)
		mutations <- mutate mmc rng (V.take (mmcMutators mmc) sortedPop)
		let pop' = survivors <> offspring <> mutations
		    sizes = sort . V.toList $ gSize <$> pop'
		    quartile n = case (V.length pop' * n) `quotRem` 4 of
		    	(q, r) -> fromIntegral (sizes !!  q   ) * (fromIntegral (4-r) / 4)
		    	        + fromIntegral (sizes !! (q+1)) * (fromIntegral    r  / 4)
		atomically $ modifyTVar overviewRef \overview -> overview
			{ goID = goID overview + 1
			, goLevelsPlayed = 0
			, goLevelsToPlay = V.length pop' * length gslks
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
what'sBad pop e = (-eViruses e, eFramesToLastKill e, gSize (pop V.! eID e))

breed :: MsMendelConfig -> GenIO -> Vector Genome -> IO (Vector Genome)
breed mmc rng pop
	| V.length pop < 2 = pure V.empty
	| otherwise = V.replicateM (mmcOffspring mmc) do
		(g, g') <- chooseTwo
		shuffledIndices <- uniformShuffle ((Left <$> V.generate (gSize g) id) <> (Right <$> V.generate (gSize g') id)) rng
		len <- min (mmcMaxGenomeSize mmc) . (1+) <$> uniformVI' rng shuffledIndices
		let (indices, indices') = V.partitionWith id (V.take len shuffledIndices)
		liftJ2 gAppend (gIndices g (V.toList indices)) (gIndices g' (V.toList indices'))
	where
	chooseTwo = do
		[a, b] <- replicateM 2 (uniformVI' rng pop)
		if a == b then chooseTwo else pure (pop V.! a, pop V.! b)

mutate :: MsMendelConfig -> GenIO -> Vector Genome -> IO (Vector Genome)
mutate mmc rng pop = do
	ins <- V.replicateM (mmcGeneReplacements mmc) replaceGene
	del <- if any (>0) (gSize <$> pop)
		then V.replicateM (mmcGeneDeletions mmc) deleteGene
		else pure V.empty -- should never happen
	pat <- V.replicateM (mmcPatternToggles mmc) togglePattern
	sco <- V.replicateM (mmcScoreToggles mmc) toggleScore
	adj <- V.replicateM (mmcScoreAdjustments mmc) adjustScore
	pure $ mconcat [ins, del, pat, sco, adj]
	where
	replaceGene = do
		g <- uniformV' rng pop
		let sz = gSize g
		n <- uniformIndex sz
		g' <- gIndices g $ [0..n-1] ++ [n+1..sz-1]
		g'' <- newJeffreysGenome rng (mmcPatternWidth mmc) (mmcPatternHeight mmc) 1
		gAppend g' g''
	deleteGene = do
		g <- uniformV' rng pop
		let sz = gSize g
		if sz <= 1 then deleteGene else do
			n <- uniformIndex sz
			gIndices g $ [0..n-1] ++ [n+1..sz-1]
	togglePattern = do
		g <- uniformV' rng pop >>= gClone
		pat <- uniformPattern g
		chan <- uniformV' rng allChannels
		x <- uniformIndex (mmcPatternWidth mmc)
		y <- uniformIndex (mmcPatternHeight mmc)
		case chan of
			Left  color -> gSetColorPattern g pat color x y . not $ gGetColorPattern g pat color x y
			Right shape -> gSetShapePattern g pat shape x y . not $ gGetShapePattern g pat shape x y
		pure g
	toggleScore = do
		g <- uniformV' rng pop >>= gClone
		pat <- uniformPattern g
		gSetPatternScore g pat . negate $ gGetPatternScore g pat
		pure g
	adjustScore = do
		g <- uniformV' rng pop >>= gClone
		pat <- uniformPattern g
		let range = log (mmcMaxScoreAdjustmentFactor mmc)
		factor <- exp <$> uniformRM (-range, range) rng
		gSetPatternScore g pat . (factor*) $ gGetPatternScore g pat
		pure g
	uniformPattern = uniformIndex . gSize
	uniformIndex n = uniformRM (0, n-1) rng

allChannels :: Vector (Either (WithSentinels Color) (WithSentinels Shape))
allChannels = fmap Left allColorSentinels <> fmap Right allShapeSentinels

allColorSentinels :: Vector (WithSentinels Color)
allColorSentinels = addSentinels [minBound..maxBound]

allShapeSentinels :: Vector (WithSentinels Shape)
allShapeSentinels = addSentinels [Virus, Disconnected, East, West] -- North, South = Disconnected

addSentinels :: [a] -> Vector (WithSentinels a)
addSentinels as = V.fromList $ map NonSentinel as ++ [EmptySentinel, OutOfBoundsSentinel]
