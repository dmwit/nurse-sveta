module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Bits
import Data.Char
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.IORef
import Data.List
import Data.Ord
import Data.Time
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
import System.IO
import System.IO.Error
import System.Random.MWC
import System.Random.MWC.Distributions
import System.Random.Stateful (uniformFloat01M)
import System.Mem
import Util

import qualified Data.ByteString.Lazy as LBS
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

data GenomeConfig = GenomeConfig
	{ gcInitialPatterns :: Int
	, gcMaxPatterns :: Int
	} deriving (Eq, Ord, Read, Show, Generic)

instance FromJSON GenomeConfig where parseJSON = genericParseJSON (dashParseJSONOptions "GenomeConfig" "gc")

data MsMendelConfig = MsMendelConfig
	{ mmcInitialEvaluationThreads :: Int
	, mmcInitialEvolutionThreads :: Int
	, mmcInitialPopulation :: Int
	, mmcPillCycleLength :: Int
	, mmcEvaluationRateLimit :: Int
	, mmcRunsPerGeneration :: Int
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
	, mmcBulkPatternToggles :: Int
	, mmcTypicalPatternToggleBatchSize :: Float
	, mmcMaxLevel :: Int
	, mmcGenomeConfig :: HashMap ConvolutionSize GenomeConfig
	, mmcMaxPillsPerKill :: Int
	} deriving (Eq, Ord, Read, Show, Generic)

instance FromJSON MsMendelConfig where parseJSON = genericParseJSON (dashParseJSONOptions "MsMendelConfig" "mmc")

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

dashParseJSONOptions :: String -> String -> Options
dashParseJSONOptions typeName prefix = defaultOptions
	{ fieldLabelModifier = \fieldName -> case stripPrefix prefix fieldName of
		Just s -> drop 1 [c' | c <- s, c' <- ['-' | isUpper c] ++ [toLower c]]
		Nothing -> error $ "unexpected field name " ++ show fieldName ++ " in parseJSON @" ++ typeName
	, allowOmittedFields = False
	, rejectUnknownFields = True
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
	    	let scores = iEvaluate (jIndividual job) cur next
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

-- use the Jeffreys prior for Bernoulli distributions, β(½,½), to choose the
-- Bernoulli parameter
newJeffreysGenome :: GenIO -> ConvolutionSize -> Int -> IO Genome
newJeffreysGenome rng cs n = newGenome cs n . realToFrac =<< beta 0.5 0.5 rng

newJeffreysIndividual :: GenIO -> HashMap ConvolutionSize GenomeConfig -> IO Individual
newJeffreysIndividual rng = HM.traverseWithKey \cs -> newJeffreysGenome rng cs . gcInitialPatterns

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
	tvNew (tTop t) (readTVarIO overviewRef >>= refresh) (evolutionThread mmc jobs dir replies overviewRef rng pop)

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

evolutionThread :: MsMendelConfig -> MVar Job -> FilePath -> MVar Evaluation -> TVar GenerationOverview -> GenIO -> Vector Individual -> StatusCheck -> IO ()
evolutionThread mmc jobs dir replies overviewRef rng pop0 sc = go pop0 where
	go pop = do
		readTVarIO overviewRef >>= savePopulation dir pop . goID
		scIO_ sc
		gslks <- concat <$> forM [0..mmcMaxLevel mmc] \lev -> do
			forM [1..mmcRunsPerGeneration mmc] \_ -> do
				gs <- initialState (ExactLevel rng lev)
				lk <- replicateM (mmcPillCycleLength mmc) (sampleRNG' rng)
				pure (gs, lk)
		tid <- forkIO $ forM_ gslks \(gs0, lks) -> V.iforM_ pop \i ind -> do
			gs <- cloneGameState gs0
			putMVar jobs Job
				{ jIndividual = ind
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

		getCurrentTime >>= print
		report "sortedIDs" sortedIDs
		report "eViruses" (eViruses <$> frozenEvals)
		report "eFramesToLastKill" (eFramesToLastKill <$> frozenEvals)
		report "sizes" (iSize <$> sortedPop)
		putStrLn ""
		hFlush stdout

		offspring <- breed mmc rng (V.take (mmcBreeders mmc) sortedPop)
		mutations <- mutate mmc rng (V.take (mmcMutators mmc) sortedPop)
		let pop' = survivors <> offspring <> mutations
		    sizes = sort . V.toList $ iSize <$> pop'
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

initializePopulation :: MsMendelConfig -> FilePath -> GenIO -> IO (Int, Vector Individual)
initializePopulation mmc dir rng = do
	let generationFilename = dir </> "latest.json"
	handle (missing generationFilename) do
		bsGeneration <- LBS.readFile generationFilename
		handle (corrupt generationFilename) do
			generation <- throwDecode bsGeneration
			let specsFilename = dir </> show generation <.> "json"
			handle (missing ("WARNING: " ++ specsFilename)) do
				bsSpecs <- LBS.readFile specsFilename
				handle (corrupt specsFilename) do
					RecordOfVectors specs <- throwDecode bsSpecs
					population <- traverse iFromSpec specs
					pure (generation, population)
	where
	corrupt fp (AesonException e) = do
		putStrLn $ "WARNING: creating a fresh population because " ++ fp ++ " was corrupt: " ++ e
		freshPopulation
	missing prefix e = if isDoesNotExistError e
		then do
			putStrLn $ prefix ++ " does not exist; creating a fresh population"
			freshPopulation
		else throw e
	freshPopulation = fmap ((,)0) . V.replicateM (mmcInitialPopulation mmc) $
		newJeffreysIndividual rng (mmcGenomeConfig mmc)

savePopulation :: FilePath -> Vector Individual -> Int -> IO ()
savePopulation dir pop generation = do
	saveAtomically dir (show generation <.> "json") (RecordOfVectors (iSpec <$> pop))
	saveAtomically dir "latest.json" generation

saveAtomically :: ToJSON a => FilePath -> FilePath -> a -> IO ()
saveAtomically dir nm a = do
	encodeFile (dir </> "." ++ nm) a
	renameFile (dir </> "." ++ nm) (dir </> nm)

iSize :: Individual -> Int
iSize = sum . fmap gSize

iGenome :: Individual -> ConvolutionSize -> IO Genome
iGenome ind cs = case HM.lookup cs ind of
	Just g -> pure g
	Nothing -> newGenome cs 0 0

what'sBad :: Vector Individual -> Evaluation -> (Int, Int, Int)
what'sBad pop e = (-eViruses e, eFramesToLastKill e, iSize (pop V.! eID e))

breed :: MsMendelConfig -> GenIO -> Vector Individual -> IO (Vector Individual)
breed mmc rng pop
	| V.length pop < 2 = pure V.empty
	| otherwise = V.replicateM (mmcOffspring mmc) do
		(ind, ind') <- chooseTwo
		HM.traverseWithKey (\cs cfg -> liftJ2 (breedGenome cfg) (iGenome ind cs) (iGenome ind' cs)) (mmcGenomeConfig mmc)
	where
	chooseTwo = do
		[a, b] <- replicateM 2 (uniformVI' rng pop)
		if a == b then chooseTwo else pure (pop V.! a, pop V.! b)
	breedGenome cfg g g' = do
		shuffledIndices <- uniformShuffle ((Left <$> V.generate (gSize g) id) <> (Right <$> V.generate (gSize g') id)) rng
		len <- min (gcMaxPatterns cfg) . (1+) <$> uniformVI' rng shuffledIndices
		let (indices, indices') = V.partitionWith id (V.take len shuffledIndices)
		liftJ2 gAppend (gIndices g (V.toList indices)) (gIndices g' (V.toList indices'))

mutate :: MsMendelConfig -> GenIO -> Vector Individual -> IO (Vector Individual)
mutate mmc rng pop = do
	ins <- V.replicateM (mmcGeneReplacements mmc) replaceGene
	del <- if any (>0) (iSize <$> pop)
		then V.replicateM (mmcGeneDeletions mmc) deleteGene
		else pure V.empty -- should never happen
	pat <- V.replicateM (mmcPatternToggles mmc) togglePattern
	sco <- V.replicateM (mmcScoreToggles mmc) toggleScore
	adj <- V.replicateM (mmcScoreAdjustments mmc) adjustScore
	blk <- V.replicateM (mmcBulkPatternToggles mmc) bulkPatternToggle
	pure $ mconcat [ins, del, pat, sco, adj, blk]
	where
	replaceGene = onUniformIndividual'sGene rng pop \cs n sz g -> do
		g' <- gIndices g $ [0..n-1] ++ [n+1..sz-1]
		g'' <- newJeffreysGenome rng cs 1
		gAppend g' g''
	deleteGene = do
		ind <- uniformV' rng pop
		if iSize ind <= 1 then deleteGene else onUniformGene rng ind \_cs n sz g ->
			gIndices g $ [0..n-1] ++ [n+1..sz-1]
	togglePattern = onUniformIndividual'sGene rng pop \cs _n _sz g_ -> do
		g <- gClone g_
		pat <- uniformPattern g
		chan <- uniformV' rng allChannels
		x <- uniformIndex (csWidth cs)
		y <- uniformIndex (csHeight cs)
		case chan of
			Left  color -> gSetColorPattern g pat color x y . not $ gGetColorPattern g pat color x y
			Right shape -> gSetShapePattern g pat shape x y . not $ gGetShapePattern g pat shape x y
		pure g
	toggleScore = onUniformIndividual'sGene rng pop \_cs _n _sz g_ -> do
		g <- gClone g_
		pat <- uniformPattern g
		gSetPatternScore g pat . negate $ gGetPatternScore g pat
		pure g
	adjustScore = onUniformIndividual'sGene rng pop \_cs _n _sz g_ -> do
		g <- gClone g_
		pat <- uniformPattern g
		let range = log (mmcMaxScoreAdjustmentFactor mmc)
		factor <- exp <$> uniformRM (-range, range) rng
		gSetPatternScore g pat . (factor*) $ gGetPatternScore g pat
		pure g
	bulkPatternToggle = onUniformIndividual'sGene rng pop \cs _n _sz g_ -> do
		g <- gClone g_
		pat <- uniformPattern g
		pat' <- newJeffreysGenome rng cs 1
		let loop = do
		    	x <- uniformIndex (csWidth cs)
		    	y <- uniformIndex (csHeight cs)
		    	for_ allColorsWithSentinels \c -> gSetColorPattern g pat c x y (gGetColorPattern pat' 0 c x y)
		    	for_ allShapesWithSentinels \s -> gSetShapePattern g pat s x y (gGetShapePattern pat' 0 s x y)
		    	n <- uniformFloat01M rng
		    	when (n > pDone) loop
		    -- this calculation isn't exactly correct because we make no
		    -- attempt to choose unique locations each time, but meh, close
		    -- enough
		    pDone = recip (mmcTypicalPatternToggleBatchSize mmc)
		g <$ loop
	uniformPattern = uniformIndex . gSize
	uniformIndex n = uniformRM (0, n-1) rng

uniformGene :: GenIO -> Individual -> IO (ConvolutionSize, Int, Int, Genome)
uniformGene rng ind = uniformV' rng . V.fromList $
	[ (cs, i, sz, g)
	| (cs, g) <- HM.toList ind
	, let sz = gSize g
	, i <- [0..sz-1]
	]

onUniformGene :: GenIO -> Individual -> (ConvolutionSize -> Int -> Int -> Genome -> IO Genome) -> IO Individual
onUniformGene rng ind f = do
	(cs, n, sz, g) <- uniformGene rng ind
	flip (HM.insert cs) ind <$> f cs n sz g

onUniformIndividual'sGene :: GenIO -> Vector Individual -> (ConvolutionSize -> Int -> Int -> Genome -> IO Genome) -> IO Individual
onUniformIndividual'sGene rng pop f = do
	ind <- uniformV' rng pop
	onUniformGene rng ind f

allChannels :: Vector (Either (WithSentinels Color) (WithSentinels Shape))
allChannels = fmap Left allColorSentinels <> fmap Right allShapeSentinels

allColorSentinels :: Vector (WithSentinels Color)
allColorSentinels = addSentinels [minBound..maxBound]

allShapeSentinels :: Vector (WithSentinels Shape)
allShapeSentinels = addSentinels [Virus, Disconnected, East, West] -- North, South = Disconnected

addSentinels :: [a] -> Vector (WithSentinels a)
addSentinels as = V.fromList $ map NonSentinel as ++ [EmptySentinel, OutOfBoundsSentinel]
