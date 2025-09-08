module Main where

import GI.Gtk
import Ms.Mendel
import Nurse.Sveta.STM
import Nurse.Sveta.Widget

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
	mmc <- loadConfiguration
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

data Job = Job
	{ jIndividual :: Individual
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
evaluationThread mmc jobs psmRef sc = makeLogger mmc "evaluation" >>= \log -> createSystemRandom >>= \rng -> forever do
	log "scIO_ sc"
	scIO_ sc
	log "takeMVar jobs"
	job <- takeMVar jobs
	-- we need to make a copy so that scIO below can put the original game back
	-- into the queue
	log "cloneGameState (jGame job)"
	gs <- cloneGameState (jGame job)
	let moveLoop pills frames [] = moveLoop pills frames (jLookaheads job)
	    moveLoop pills frames (lk:lks) = log "stopMoving pills gs" >> stopMoving pills gs >>= \b -> if b then pure frames else do
	    	log "scIO sc (putMVar jobs job)"
	    	scIO sc (putMVar jobs job)
	    	log "mfreeze (board gs)"
	    	cur <- mfreeze (board gs)
	    	-- TODO: would be nice to do this after playing the move, since we
	    	-- only really ever see this in detail when evaluation threads have
	    	-- finished a generation's games and are waiting for their peers,
	    	-- but getting the lookahead right is obnoxious
	    	log $ "atomically $ writeTVar psmRef " ++ show PSM { psmBoard = cur, psmLookahead = Just lk, psmOverlay = [] }
	    	atomically $ writeTVar psmRef PSM { psmBoard = cur, psmLookahead = Just lk, psmOverlay = [] }
	    	log $ "readIORef (framesPassed/pillsUsed/virusesKilled gs)"
	    	fp <- readIORef (framesPassed gs)
	    	pu <- readIORef (pillsUsed gs)
	    	vk <- readIORef (virusesKilled gs)
	    	log "mapproxReachable"
	    	placements <- mapproxReachable (board gs) (fp .&. 1 /= fromEnum (originalSensitive gs)) (gravity (speed gs) pu)
	    	let moves = V.fromList . HM.toList . HM.fromListWith shorterPath $
	    	    	[(mpPill placement lk, path) | (placement, path) <- HM.toList placements]
	    	log "for moves \\(pill, path) -> do"
	    	next <- for moves \(pill, path) -> do
	    		log "\tcloneGameState gs"
	    		gs' <- cloneGameState gs
	    		log $ "\tplayMove gs' " ++ ppAeson path ++ " " ++ ppPill pill
	    		playMove gs' path pill
	    		log "mfreeze (board gs')"
	    		mfreeze (board gs')
	    	let scores = iEvaluate (jIndividual job) cur next
	    	    bestScore = V.maximum scores
	    	    bestIndices = V.findIndices (bestScore==) scores
	    	    rateLimit = mmcEvaluationRateLimit mmc
	    	log "(moves V.!) <$> uniformV' rng bestIndices"
	    	(pill, path) <- (moves V.!) <$> uniformV' rng bestIndices
	    	log $ "playMove gs " ++ ppAeson path ++ " " ++ ppPill pill
	    	playMove gs path pill
	    	log "readIORef (virusesKilled gs"
	    	vk' <- readIORef (virusesKilled gs)
	    	log "(pills', frames') <- if vk' > vk then readIORef else (pills, frames)"
	    	(pills', frames') <- if vk' > vk
	    		then liftM2 (,) (readIORef (pillsUsed gs)) (readIORef (framesPassed gs))
	    		else pure (pills, frames)
	    	log "threadDelay"
	    	when (rateLimit > 0) (threadDelay rateLimit)
	    	log "moveLoop pills' frames' lks"
	    	moveLoop pills' frames' lks
	log "moveLoop 0 0 []"
	frames <- moveLoop 0 0 []
	log "readIORef (virusesKilled gs)"
	vk <- readIORef (virusesKilled gs)
	log $ "putMVar (jReply job) " ++ show Evaluation { eID = jID job, eViruses = vk, eFramesToLastKill = frames }
	putMVar (jReply job) Evaluation
		{ eID = jID job
		, eViruses = vk
		, eFramesToLastKill = frames
		}
	where
	stopMoving pills gs = finished gs `orM` do
		pills' <- readIORef (pillsUsed gs)
		pure (pills' > pills + mmcMaxPillsPerKill mmc)

data GenerationOverview = GenerationOverview
	{ goID :: Int
	, goPopulationSize :: Int
	, goRunsPerGeneration :: Int
	, goLevelsPlayed :: Int
	, goLevelsToPlay :: Int
	, goBestSoFar :: Maybe Evaluation
	, goWorstSoFar :: Maybe Evaluation
	, goMinSize, goFirstQuartileSize, goMedianSize, goLastQuartileSize, goMaxSize :: Double
	} deriving (Eq, Ord, Read, Show)

goCurrentLevelEstimate, goVirusesAvailableEstimate :: GenerationOverview -> Int
goCurrentLevelEstimate go = goLevelsPlayed go `div` goPopulationSize go `div` goRunsPerGeneration go
goVirusesAvailableEstimate go = (goRunsPerGeneration go *) . (\lev -> 2 * (lev+1) * (lev+2)) . goCurrentLevelEstimate $ go

data Table = Table
	{ tNextRow :: IORef Int32
	, tRefreshRef :: IORef (GenerationOverview -> IO ())
	, tTop :: Grid
	}

newTable :: IO Table
newTable = pure Table <*> newIORef 0 <*> newIORef (\_ -> pure ()) <*> new Grid []

tAddRow :: Table -> T.Text -> (GenerationOverview -> T.Text) -> IO ()
tAddRow t desc fVal = do
	descLbl <- new Label [#label := desc, #halign := AlignStart]
	valLbl <- new Label [#halign := AlignEnd]
	modifyIORef (tRefreshRef t) \f go -> do
		set valLbl [#label := fVal go]
		f go
	row <- readIORef (tNextRow t)
	#attach (tTop t) descLbl 0 row 1 1
	#attach (tTop t)  valLbl 1 row 1 1
	writeIORef (tNextRow t) (row+1)

evolutionThreadView :: MsMendelConfig -> MVar Job -> IO ThreadView
evolutionThreadView mmc jobs = do
	rng <- createSystemRandom
	dir <- getXdgDirectory XdgData "ms-mendel"
	log <- makeLogger mmc "evolution"
	createDirectoryIfMissing True dir
	(generation, pop) <- initializePopulation mmc dir rng
	replies <- newEmptyMVar
	overviewRef <- newTVarIO GenerationOverview
		{ goID = generation
		, goPopulationSize = V.length pop
		, goRunsPerGeneration = mmcRunsPerGeneration mmc
		, goLevelsPlayed = 0
		, goLevelsToPlay = V.length pop * (mmcMaxLevel mmc + 1) * mmcRunsPerGeneration mmc
		, goBestSoFar = Nothing
		, goWorstSoFar = Nothing
		, goMinSize = 0
		, goFirstQuartileSize = 0
		, goMedianSize = 0
		, goLastQuartileSize = 0
		, goMaxSize = 0
		}

	log . show =<< getCurrentTime
	log $ "generation: " ++ show generation
	log $ "configuration: " ++ ppAeson mmc
	log ""

	t <- newTable
	tAddRow t "generation" (tshow . goID)
	tAddRow t "population size" (tshow . goPopulationSize)
	tAddRow t "boards to evaluate" (tshow . goLevelsToPlay)
	tAddRow t "boards evaluated" (tshow . goLevelsPlayed)
	tAddRow t "currently on level" (tshow . goCurrentLevelEstimate)
	tAddRow t "scaled virus kills" (scaleViruses mmc)
	tAddRow t "viruses available to kill" (tshow . goVirusesAvailableEstimate)
	tAddRow t "most viruses killed this generation" (maybe "" (tshow . eViruses) . goBestSoFar)
	tAddRow t "\tframes required" (maybe "" (tshow . eFramesToLastKill) . goBestSoFar)
	tAddRow t "\tminutes required" (maybe "" (asMinutes . eFramesToLastKill) . goBestSoFar)
	tAddRow t "least viruses killed this generation" (maybe "" (tshow . eViruses) . goWorstSoFar)
	tAddRow t "\tframes required" (maybe "" (tshow . eFramesToLastKill) . goWorstSoFar)
	tAddRow t "\tminutes required" (maybe "" (asMinutes . eFramesToLastKill) . goWorstSoFar)
	tAddRow t "genome size by percentile" (const "")
	tAddRow t "\tmin" (tshow . goMinSize)
	tAddRow t "\t25%" (tshow . goFirstQuartileSize)
	tAddRow t "\t50%" (tshow . goMedianSize)
	tAddRow t "\t75%" (tshow . goLastQuartileSize)
	tAddRow t "\tmax" (tshow . goMaxSize)

	refresh <- readIORef (tRefreshRef t)
	tvNew (tTop t) (readTVarIO overviewRef >>= refresh) (evolutionThread mmc log jobs dir replies overviewRef rng pop)

scaleViruses :: MsMendelConfig -> GenerationOverview -> T.Text
scaleViruses mmc overview = case goBestSoFar overview of
	Nothing -> ""
	Just e -> tshow lo <> "-" <> tshow hi where
		maxVir = 2 * (mmcMaxLevel mmc + 1) * (mmcMaxLevel mmc + 2)
		scale = fromIntegral maxVir / fromIntegral (goVirusesAvailableEstimate overview)
		floatViruses = fromIntegral (eViruses e)
		lo = max 0      . floor   $ scale * (floatViruses - 0.5)
		hi = min maxVir . ceiling $ scale * (floatViruses + 0.5)

asMinutes :: Int -> T.Text
asMinutes frames = tshow wholeMinutes <> ":" <> zeroPad 2 (tshow wholeSeconds) <> "." <> zeroPad 3 (tshow wholeMillis) where
	minutes = fromIntegral frames / ntscFrameRate / 60
	wholeMinutes = floor minutes
	seconds = 60 * (minutes - fromIntegral wholeMinutes)
	wholeSeconds = floor seconds
	millis = 1000 * (seconds - fromIntegral wholeSeconds)
	wholeMillis = round millis
	zeroPad n t = T.replicate (n - T.length t) "0" <> t

evolutionThread :: MsMendelConfig -> (String -> IO ()) -> MVar Job -> FilePath -> MVar Evaluation -> TVar GenerationOverview -> GenIO -> Vector Individual -> StatusCheck -> IO ()
evolutionThread mmc log jobs dir replies overviewRef rng pop0 sc = makeLogger mmc "evolution-details" >>= go pop0 where
	go pop logDetails = do
		logDetails "readTVarIO overviewRef >>= savePopulation dir pop . goID"
		readTVarIO overviewRef >>= savePopulation dir pop . goID
		logDetails "scIO_ sc"
		scIO_ sc
		logDetails "forM [0..mmcMaxLevel mmc] \\lev -> do"
		gslks <- concat <$> forM [0..mmcMaxLevel mmc] \lev -> do
			logDetails "\tforM [1..mmcRunsPerGeneration mmc] \\_ -> do"
			forM [1..mmcRunsPerGeneration mmc] \_ -> do
				logDetails $ "\t\tinitialState (ExactLevel rng " ++ show lev
				gs <- initialState (ExactLevel rng lev)
				logDetails $ "\t\treplicateM cycleLength (sampleRNG' rng)"
				lk <- replicateM (mmcPillCycleLength mmc) (sampleRNG' rng)
				logDetails $ "\t\tpure (gs, lk)"
				pure (gs, lk)
		logDetails "forkIO $ forM_ gslks \\(gs0, lks) -> V.iforM_ pop \\i ind -> do"
		tid <- forkIO $ forM_ gslks \(gs0, lks) -> V.iforM_ pop \i ind -> do
			logDetails "\t[forkIO]cloneGameState gs0"
			gs <- cloneGameState gs0
			logDetails "\t[forkIO]putMVar jobs Job"
			putMVar jobs Job
				{ jIndividual = ind
				, jGame = gs
				, jLookaheads = lks
				, jID = i
				, jReply = replies
				}
		logDetails "VM.generate (V.length pop) \\i -> Evaluation { eID = i, eViruses = 0, eFramesToLastKill = 0 }"
		evals <- VM.generate (V.length pop) \i -> Evaluation { eID = i, eViruses = 0, eFramesToLastKill = 0 }
		logDetails "forM_ gslks \\_ -> do"
		forM_ gslks \_ -> do
			logDetails "\tatomically $ modifyTVar overviewRef (goWorstSoFar ~= Nothing)"
			atomically $ modifyTVar overviewRef \overview -> overview { goWorstSoFar = Nothing }
			logDetails "forM_ pop"
			forM_ pop \_ -> do
				logDetails "\tscIO sc ..."
				scIO sc do
					forkIO . forever $ takeMVar replies
					killThread tid
				logDetails "\ttakeMVar replies"
				deval <- takeMVar replies
				logDetails "\tVM.modify evals (deval<>) (eID deval)"
				VM.modify evals (deval<>) (eID deval)
				logDetails "\tVM.read evals (eID deval)"
				eval <- VM.read evals (eID deval)
				logDetails "\tatomically $ modifyTVar overviewRef (goLevelsPlayed += 1, goBestSoFar ~= Just ..., goWorstSoFar ~= Just ...)"
				atomically $ modifyTVar overviewRef \overview -> overview
					{ goLevelsPlayed = goLevelsPlayed overview + 1
					, goBestSoFar = Just $ maybe eval (minOn (what'sBad pop) eval) (goBestSoFar overview)
					, goWorstSoFar = Just $ maybe eval (maxOn (what'sBad pop) eval) (goWorstSoFar overview)
					}
		logDetails "V.sortBy what'sBad evals"
		V.sortBy (comparing (what'sBad pop)) evals
		logDetails "V.freeze evals"
		frozenEvals <- V.freeze evals
		let sortedIDs = eID <$> frozenEvals
		    sortedPop = V.backpermute pop sortedIDs
		    survivors = V.take (mmcSurvivors mmc) sortedPop
		    report nm val = log (nm ++ ": " ++ show val)

		log . show =<< getCurrentTime
		report "sortedIDs" sortedIDs
		report "eViruses" (eViruses <$> frozenEvals)
		report "eFramesToLastKill" (eFramesToLastKill <$> frozenEvals)
		report "sizes" (iSize <$> sortedPop)
		log ""

		logDetails "breed mmc rng sortedPop"
		offspring <- breed mmc rng (V.take (mmcBreeders mmc) sortedPop)
		logDetails "mutate mmc rng sortedPop"
		mutations <- mutate mmc rng (V.take (mmcMutators mmc) sortedPop)
		logDetails "addGenes mmc rng sortedPop"
		additions <- addGenes mmc rng (V.take (mmcMutators mmc) sortedPop)
		let pop' = mconcat [survivors, offspring, mutations, additions]
		    sizes = sort . V.toList $ iSize <$> pop'
		    quartile n = case (V.length pop' * n) `quotRem` 4 of
		    	(q, r) -> fromIntegral (sizes !!  q   ) * (fromIntegral (4-r) / 4)
		    	        + fromIntegral (sizes !! (q+1)) * (fromIntegral    r  / 4)
		logDetails "atomically $ modifyTVar overviewRef ..."
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
		go pop' logDetails

initializePopulation :: MsMendelConfig -> FilePath -> GenIO -> IO (Int, Vector Individual)
initializePopulation mmc dir rng = loadPopulation mmc dir >>= \case
	Right genPop -> pure genPop
	Left failure -> do
		putStrLn case failure of
			MissingGeneration fp -> fp ++ " does not exist; creating a fresh population"
			MissingPopulation fp -> "WARNING: " ++ fp ++ " does not exist; creating a fresh population"
			Corrupt fp e -> "WARNING: creating a fresh population because " ++ fp ++ " was corrupt: " ++ e
		fmap ((,)0) . V.replicateM (mmcInitialPopulation mmc) $ newJeffreysIndividual mmc rng

what'sBad :: Vector Individual -> Evaluation -> (Int, Int, Int)
what'sBad pop e = (-eViruses e, eFramesToLastKill e, iSize (pop V.! eID e))
