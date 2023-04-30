module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Encoding
import Data.Bits
import Data.Fixed
import Data.Foldable
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.IntMap (IntMap)
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Time (UTCTime)
import Data.Traversable
import Dr.Mario.Model
import GI.Gtk as G
import Numeric
import Nurse.Sveta.Cairo
import Nurse.Sveta.STM
import Nurse.Sveta.STM.BatchProcessor
import Nurse.Sveta.Tomcats
import Nurse.Sveta.Torch
import Nurse.Sveta.Util
import Nurse.Sveta.Widget
import Paths_nurse_sveta
import System.Clock.Seconds
import System.Directory
import System.Environment
-- TODO: this is now built into the directory package, we should switch to that
import System.Environment.XDG.BaseDir
import System.FilePath
import System.IO
import System.IO.Error
import System.Mem
import System.Process
import System.Random.MWC
import Text.Printf
import Util

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Vector as V
import qualified GI.Cairo.Render as G

-- ╭╴w╶──────────────────────╮
-- │╭╴top╶──────────────────╮│
-- ││╭╴gen╶╮╭╴txt╶──╮╭╴rep╶╮││
-- ││╰─────╯│╭╴inf╶╮│╰─────╯││
-- ││       │╰─────╯│       ││
-- ││       │╭╴bur╶╮│       ││
-- ││       │╰─────╯│       ││
-- ││       │╭╴trn╶╮│       ││
-- ││       │╰─────╯│       ││
-- ││       │╭╴log╶╮│       ││
-- ││       │╰─────╯│       ││
-- ││       ╰───────╯       ││
-- │╰───────────────────────╯│
-- ╰─────────────────────────╯
main :: IO ()
main = do
	torchPlusGtkFix
	app <- new Application []
	(runName, args) <- getArgs >>= \case
		[] -> fail "USAGE: nurse-sveta RUN_NAME"
		runName:args -> pure (runName, args)

	on app #activate $ do
		inferenceProcedure <- newProcedure 100
		loggingProcedure <- newProcedure 10000
		bureaucracyLock <- newMVar newBureaucracyGlobalState
		netUpdate <- newTVarIO Nothing
		mainRef <- newIORef Nothing

		top <- new Box [#orientation := OrientationHorizontal, #spacing := 10]
		txt <- new Box [#orientation := OrientationVertical]

		gen <- newThreadManager "generation" Green (generationThreadView inferenceProcedure)
		inf <- newThreadManager "inference" OS (inferenceThreadView inferenceProcedure netUpdate)
		bur <- newThreadManager "bureaucracy" Green (bureaucracyThreadView loggingProcedure bureaucracyLock)
		trn <- newThreadManager "training" OS (trainingThreadView loggingProcedure netUpdate)
		log <- newThreadManager "logging" Green (loggingThreadView loggingProcedure runName)
		tmStartThread bur
		tmStartThread trn
		tmStartThread log
		tmWidget gen >>= #append top
		tmWidget inf >>= #append txt
		tmWidget bur >>= #append txt
		tmWidget trn >>= #append txt
		tmWidget log >>= #append txt
		#append top txt

		w <- new Window $ tail [undefined
			, #title := "Nurse Sveta"
			, #application := app
			, #child := top
			, #defaultWidth := 1500
			, #defaultHeight := 1000
			, On #closeRequest $ readIORef mainRef >>= \case
				Just{} -> pure False
				Nothing -> do
					let quitIfAppropriate = readIORef mainRef >>= \case
					    	Nothing -> fail "the impossible happened: a thread manager finished dying before thread managers started dying"
					    	Just [] -> fail "the impossible happened: after all the thread managers died, the app didn't exit AND another thread manager died"
					    	Just ([]:_) -> fail "the impossible happened: all the thread managers in one dependency level died, but didn't start killing managers in the next dependency level"
					    	-- performGC to run finalizers
					    	Just [[_]] -> performGC >> #quit app
					    	Just ([_]:tms:tmss) -> writeIORef mainRef (Just (tms:tmss)) >> traverse_ (\tm -> tm `tmDieThen` quitIfAppropriate) tms
					    	Just ((_:tms):tmss) -> writeIORef mainRef (Just (tms:tmss))
					-- dependencies reified here:
					-- * generation threads may block if inference threads die too early
					-- * training threads may block if logging threads die too early
					-- the undefined looks scary, but it's just to kick off the
					-- recursion, it's completely ignored
					writeIORef mainRef (Just [[undefined], [gen, trn], [inf, log, bur]])
					quitIfAppropriate
					pure True
			]
		#show w
	() <$ #run app (Just args)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- this generation is in the sense of creation
data GenerationThreadState = GenerationThreadState
	{ summary :: SearchSummary
	, requestedConfiguration :: SearchConfiguration
	, currentConfiguration :: Stable SearchConfiguration
	} deriving Show

newGenerationThreadState :: SearchConfiguration -> GenerationThreadState
newGenerationThreadState cfg = GenerationThreadState
	{ summary = SearchSummary
		{ rootPosition = newStable (PSM (emptyBoard 8 16) Nothing [])
		, speeds = []
		}
	, requestedConfiguration = cfg
	, currentConfiguration = newStable cfg
	}

onRootPosition :: (Stable PlayerStateModel -> Stable PlayerStateModel) -> GenerationThreadState -> GenerationThreadState
onRootPosition f gts = gts { summary = s { rootPosition = f (rootPosition s) } } where
	s = summary gts

onSpeeds :: ([(T.Text, SearchSpeed)] -> [(T.Text, SearchSpeed)]) -> GenerationThreadState -> GenerationThreadState
onSpeeds f gts = gts { summary = s { speeds = f (speeds s) } } where
	s = summary gts

newSearchConfiguration :: SearchConfiguration
newSearchConfiguration = SearchConfiguration
	{ c_puct = 1 -- no idea what A0 did here
	, iterations = 800
	, typicalMoves = 40
	, priorNoise = 0.25
	}

requestConfiguration :: SearchConfiguration -> GenerationThreadState -> GenerationThreadState
requestConfiguration sc gts = gts { requestedConfiguration = sc }

acceptConfiguration :: GenerationThreadState -> GenerationThreadState
acceptConfiguration gts = gts { currentConfiguration = sTrySet (requestedConfiguration gts) (currentConfiguration gts) }

-- ╭╴top╶───────────╮
-- │╭╴psv╶╮╭╴nfo╶──╮│
-- │╰─────╯│╭╴scv╶╮││
-- │       │╰─────╯││
-- │       │╭╴spd╶╮││
-- │       │╰─────╯││
-- │       ╰───────╯│
-- ╰────────────────╯
generationThreadView :: Procedure GameState DetailedEvaluation -> IO ThreadView
generationThreadView eval = do
	genRef <- newTVarIO (newGenerationThreadState newSearchConfiguration)

	psv <- newPlayerStateView (PSM (emptyBoard 8 16) Nothing [])
	scv <- newSearchConfigurationView newSearchConfiguration (atomically . modifyTVar genRef . requestConfiguration)
	spd <- new Grid []
	nfo <- new Box [#orientation := OrientationVertical]
	top <- new Box [#orientation := OrientationHorizontal]

	psvTracker <- newTracker
	scvTracker <- newTracker

	psvWidget psv >>= #append top
	scvWidget scv >>= #append nfo
	#append nfo spd
	#append top nfo

	let refresh = do
	    	gts <- readTVarIO genRef
	    	renderSpeeds spd (speeds (summary gts))
	    	tWhenUpdated psvTracker (rootPosition (summary gts)) (psvSet psv)
	    	tWhenUpdated scvTracker (currentConfiguration gts) (scvSet scv)

	tvNew top refresh (generationThread eval genRef)

renderSpeeds :: Grid -> [(T.Text, SearchSpeed)] -> IO ()
renderSpeeds spd sss = do
	now <- Time.getCurrentTime
	let row (nm, ss) = [nm, ": ", tshow (searchIterations ss), " positions/", ms, "s = ", T.justifyRight 5 ' ' . tshow . precision 10 $ rate, " positions/s"] where
	    	dt = realToFrac . Time.diffUTCTime now . searchStart $ ss :: Double
	    	ms = T.pack (showFFloat (Just 1) (realToFrac dt) "")
	    	rate = fromIntegral (searchIterations ss) / dt
	    	precision prec n = fromInteger (round (n*prec)) / prec
	    columns = transpose (map row sss)
	unless (null sss) $ zipWithM_ updateLabel [0..] (T.unlines <$> columns)
	where
	updateLabel n t = gridUpdateLabelAt spd n 0 t (numericLabel t)

generationThread :: Procedure GameState DetailedEvaluation -> TVar GenerationThreadState -> StatusCheck -> IO ()
generationThread eval genRef sc = do
	g <- createSystemRandom
	threadSpeed <- newSearchSpeed
	gameLoop g threadSpeed
	where
	gameLoop g threadSpeed = do
		config <- atomically . stateTVar genRef $ \gts -> (requestedConfiguration gts, acceptConfiguration gts)
		let params = dmParameters config eval g
		(s0, t) <- initialTree params g
		s <- clone params s0
		gameSpeed <- newSearchSpeed
		moveLoop g config params threadSpeed gameSpeed s0 [] s t

	moveLoop g config params threadSpeed gameSpeed s0 history s t = do
		[l, r] <- map toEnum <$> replicateM 2 (uniformR (0, 2) g)
		t' <- unsafeDescend params (RNG l r) s t
		let gs = GameStep (RNG l r) mempty mempty
		boardSnapshot <- mfreeze (board s)
		atomically $ modifyTVar genRef (onRootPosition (sSet (PSM boardSnapshot (Just (l, r)) [])))
		moveSpeed <- newSearchSpeed
		searchLoop g config params s0 (gs:history) s threadSpeed gameSpeed moveSpeed t' (iterations config)

	searchLoop g config params s0 history s = innerLoop where
		innerLoop threadSpeed gameSpeed moveSpeed t 0 = descend params visitCount s t >>= \case
			Nothing -> recordGame s0 history >> gameLoop g threadSpeed
			Just (m, t') -> do
				-- it's important that we allow game trees to get garbage
				-- collected, so these `evaluate`s are about making sure we
				-- aren't holding a reference to a full game tree
				--
				-- in particular, HashMap's fmap doesn't do the thing
				stats <- evaluate (statistics t)
				childStats <- traverse (evaluate . statistics) (children t)
				unexploredStats <- traverse evaluate (unexplored t)
				let gs = GameStep m stats (childStats `HM.union` unexploredStats)
				moveLoop g config params threadSpeed gameSpeed s0 (gs:history) s t'

		innerLoop threadSpeed gameSpeed moveSpeed t n = do
			t' <- mcts params s t
			let topMoves = sortOn (negate . snd)
			    	[ (p, visitCount (statistics child))
			    	| (Placement _ p, child) <- HM.toList (children t')
			    	]
			    overlay = zip (fst <$> topMoves) [0.4, 0.1, 0.1]
			-- if mcts has thrown an error somewhere that matters, force it
			-- before we get into the critical section
			evaluate (last (show overlay))

			let [threadSpeed', gameSpeed', moveSpeed'] = ssInc <$> [threadSpeed, gameSpeed, moveSpeed]
			    speeds' = [("thread", threadSpeed), ("game", gameSpeed), ("move", moveSpeed)]

			gts <- atomically . modifyTVar genRef $ id
				. onSpeeds (const speeds')
				. onRootPosition (sOnSubterm psmOverlayL (sSet overlay))

			scIO_ sc
			case HM.size (children t') of
				0 -> recordGame s0 history >> gameLoop g threadSpeed
				_ -> innerLoop threadSpeed' gameSpeed' moveSpeed' t' (n-1)

-- [PORT] /dev/urandom
recordGame :: GameState -> [GameStep] -> IO ()
recordGame gs steps = do
	b <- mfreeze (board gs)
	now <- Time.getCurrentTime
	rand <- BS.foldr (\w s -> printf "%02x" w ++ s) "" <$> withFile "/dev/urandom" ReadMode (\h -> BS.hGet h 8)
	dir <- nsDataDir
	path <- prepareFile dir GamesPending $ show now ++ "-" ++ rand <.> "json"
	encodeFile path ((b, originalSensitive gs, speed gs), reverse steps)

data InferenceThreadState = InferenceThreadState
	{ itsThreadBatches :: SearchSpeed
	, itsThreadPositions :: SearchSpeed
	, itsNetBatches :: SearchSpeed
	, itsNetPositions :: SearchSpeed
	, itsNet :: Stable (Maybe (Integer, Net))
	, itsUseNet :: Bool
	}

itsNewNet :: InferenceThreadState -> Maybe Integer -> Bool
itsNewNet its = \case
	Nothing -> False -- if there's no net at all, there's definitely no new net
	Just n -> case sPayload (itsNet its) of
		Nothing -> True
		Just (n', _) -> n /= n'

inferenceThreadView :: Procedure GameState DetailedEvaluation -> TVar (Maybe Integer) -> IO ThreadView
inferenceThreadView eval netUpdate = do
	top <- new Box [#orientation := OrientationVertical]
	lbl <- descriptionLabel "<initializing net>"
	use <- new CheckButton [#label := "Use neural net", #active := True]
	spd <- new Grid []
	#append top lbl
	#append top use
	#append top spd

	latestNet <- inferenceThreadLoadLatestNet
	its0 <- pure InferenceThreadState
		<*> newSearchSpeed
		<*> newSearchSpeed
		<*> newSearchSpeed
		<*> newSearchSpeed
		<*> pure (newStable latestNet)
		<*> pure True
	ref <- newTVarIO its0

	on use #toggled $ do
		batches_ <- newSearchSpeed
		positions_ <- newSearchSpeed
		useNet <- G.get use #active
		atomically $ do
			its <- readTVar ref
			let (batches, positions) = case sPayload (itsNet its) of
			    	Just{} -> (batches_, positions_)
			    	Nothing -> (itsNetBatches its, itsNetPositions its)
			writeTVar ref its
				{ itsNetBatches = batches
				, itsNetPositions = positions
				, itsUseNet = useNet
				}

	tracker <- newTracker
	let refresh = do
	    	its <- readTVarIO ref
	    	tWhenUpdated tracker (itsNet its) $ \mn ->
	    		set lbl [#label := describeNet mn]
	    	renderSpeeds spd $ tail [undefined
	    		, ("positions (thread)", itsThreadPositions its)
	    		, ("positions (net)   ", itsNetPositions its)
	    		, ("batches (thread)", itsThreadBatches its)
	    		, ("batches (net)   ", itsNetBatches its)
	    		]
	tvNew top refresh (inferenceThread eval netUpdate ref)
	where
	describeNet mn = "currently loaded net: " <> case mn of
		Nothing -> "hand-crafted"
		Just (n, _) -> tshow n

data InferenceThreadStep
	= ITSLoadNet (Maybe Integer)
	| ITSProgress (IO Int)
	| ITSDie

inferenceThread :: Procedure GameState DetailedEvaluation -> TVar (Maybe Integer) -> TVar InferenceThreadState -> StatusCheck -> IO ()
inferenceThread eval netUpdate itsRef sc = forever $ do
	step <- atomically $ do
		its <- readTVar itsRef
		asum $ tail [undefined
			, ITSDie <$ scSTM sc
			, ITSLoadNet <$> (readTVar netUpdate >>= ensure (itsNewNet its))
			, case (itsUseNet its, sPayload (itsNet its)) of
				(True, Just (_, net)) -> liftEvaluation (netEvaluation net)
				(True, _) -> retry
				_ -> liftEvaluation (traverse dumbEvaluation)
			]
	case step of
		ITSProgress ion -> ion >>= \n -> atomically $ do
			its <- readTVar itsRef
			writeTVar itsRef its
				{ itsThreadBatches = ssInc (itsThreadBatches its)
				, itsNetBatches = ssInc (itsNetBatches its)
				, itsThreadPositions = ssIncBy (itsThreadPositions its) n
				, itsNetPositions = ssIncBy (itsNetPositions its) n
				}
		ITSLoadNet n -> do
			net <- traverse inferenceThreadLoadNet n
			batches <- newSearchSpeed
			positions <- newSearchSpeed
			atomically $ do
				its <- readTVar itsRef
				let its' = if itsUseNet its
				    	then its { itsNetBatches = batches, itsNetPositions = positions }
				    	else its
				writeTVar itsRef its' { itsNet = sSet net (itsNet its) }
		ITSDie -> scIO_ sc
	where
	liftEvaluation :: (forall t. Traversable t => (t GameState -> IO (t DetailedEvaluation))) -> STM InferenceThreadStep
	liftEvaluation f = ITSProgress <$> serviceCallsSTM eval (fmap (\answers -> (answers, length answers)) . f)

inferenceThreadLoadLatestNet :: IO (Maybe (Integer, Net))
inferenceThreadLoadLatestNet = do
	dir <- nsDataDir
	n <- decodeFileLoop (dir </> subdirectory Weights latestFilename)
	traverse inferenceThreadLoadNet n

-- TODO: better error handling
inferenceThreadLoadNet :: Integer -> IO (Integer, Net)
inferenceThreadLoadNet tensor = do
	dir <- nsDataDir
	-- [N]urse [S]veta [n]et
	net <- netLoadForInference (dir </> subdirectory Weights (show tensor <.> "nsn"))
	pure (tensor, net)

nsDataDir :: IO FilePath
nsDataDir = getUserDataDir "nurse-sveta"

nsRuntimeDir :: IO FilePath
nsRuntimeDir = (</> "nurse-sveta") <$> do
	fromEnv <- lookupEnv "XDG_RUNTIME_DIR"
	case fromEnv of
		Just fp -> pure fp
		Nothing -> getTemporaryDirectory

data LevelMetric = LevelMetric
	{ lmMetric :: Int
	, lmSource :: FilePath
	} deriving (Eq, Ord, Read, Show)

lmToTuple :: LevelMetric -> (Int, FilePath)
lmToTuple = liftA2 (,) lmMetric lmSource

lmFromTuple :: (Int, FilePath) -> LevelMetric
lmFromTuple = uncurry LevelMetric

instance FromJSON LevelMetric where parseJSON = fmap lmFromTuple . parseJSON
instance ToJSON LevelMetric where
	toJSON = toJSON . lmToTuple
	toEncoding lm = list id [toEncoding (lmMetric lm), toEncoding (lmSource lm)]

lmFewest :: LevelMetric -> LevelMetric -> LevelMetric
lmFewest lm lm' = if lmMetric lm < lmMetric lm' then lm else lm'

lmMost :: LevelMetric -> LevelMetric -> LevelMetric
lmMost lm lm' = if lmMetric lm > lmMetric lm' then lm else lm'

lmSum :: LevelMetric -> LevelMetric -> LevelMetric
lmSum lm lm' = LevelMetric
	{ lmMetric = lmMetric lm + lmMetric lm'
	, lmSource = "<all>"
	}

lmDouble :: LevelMetric -> Double
lmDouble = fromIntegral . lmMetric

data CategoryMetrics = CategoryMetrics
	{ cmVirusesKilled :: IntMap LevelMetric
	, cmFramesToWin   :: IntMap LevelMetric
	, cmFramesToLoss  :: IntMap LevelMetric
	} deriving (Eq, Ord, Read, Show)

vkFieldName, ftwFieldName, ftlFieldName :: Key
vkFieldName = "viruses killed"
ftwFieldName = "frames to win"
ftlFieldName = "frames to loss"

instance ToJSON CategoryMetrics where
	toJSON cm = object $ tail [undefined
		,  vkFieldName .= cmVirusesKilled cm
		, ftwFieldName .= cmFramesToWin cm
		, ftlFieldName .= cmFramesToLoss cm
		]
	toEncoding cm = pairs $ mempty
		<>  vkFieldName .= cmVirusesKilled cm
		<> ftwFieldName .= cmFramesToWin cm
		<> ftlFieldName .= cmFramesToLoss cm

instance FromJSON CategoryMetrics where
	parseJSON = withObject "CategoryMetrics" $ \v -> pure CategoryMetrics
		<*> v .:  vkFieldName
		<*> v .: ftwFieldName
		<*> v .: ftlFieldName

newCategoryMetrics :: CategoryMetrics
newCategoryMetrics = CategoryMetrics
	{ cmVirusesKilled = mempty
	, cmFramesToWin   = mempty
	, cmFramesToLoss  = mempty
	}

-- invariant: corresponding fields in cmSuperlative and cmLatest have the same keys
data CategoryMetadata = CategoryMetadata
	{ cmBest :: CategoryMetrics
	, cmLatest :: CategoryMetrics
	, cmCumulative :: CategoryMetrics
	} deriving (Eq, Ord, Read, Show)

bFieldName, rFieldName, cFieldName :: Key
bFieldName = "best"
rFieldName = "recent"
cFieldName = "cumulative"

instance ToJSON CategoryMetadata where
	toJSON cm = object $ tail [undefined
		, bFieldName .= cmBest cm
		, rFieldName .= cmLatest cm
		, cFieldName .= cmCumulative cm
		]
	toEncoding cm = pairs $ mempty
		<> bFieldName .= cmBest cm
		<> rFieldName .= cmLatest cm
		<> cFieldName .= cmCumulative cm

instance FromJSON CategoryMetadata where
	parseJSON = withObject "CategoryMetadata" $ \v -> pure CategoryMetadata
		<*> v .: bFieldName
		<*> v .: rFieldName
		<*> v .: cFieldName

newCategoryMetadata :: CategoryMetadata
newCategoryMetadata = CategoryMetadata
	{ cmBest = newCategoryMetrics
	, cmLatest = newCategoryMetrics
	, cmCumulative = newCategoryMetrics
	}

cmInsert :: FilePath -> Int -> Int -> Int -> CategoryMetadata -> CategoryMetadata
cmInsert fp startingViruses virusesKilled frames CategoryMetadata { cmBest = b, cmLatest = r, cmCumulative = c } = CategoryMetadata
	{ cmBest       = mkMetrics lmMost lmFewest lmMost b
	, cmLatest     = mkMetrics const  const    const  r
	, cmCumulative = mkMetrics lmSum  lmSum    lmSum  c
	} where
	mkMetrics vk fw fl cm = CategoryMetrics
		{ cmVirusesKilled = iw vk True   virusesKilled cmVirusesKilled cm
		, cmFramesToWin   = iw fw isWin  frames        cmFramesToWin   cm
		, cmFramesToLoss  = iw fl isLoss frames        cmFramesToLoss  cm
		}
	isWin = startingViruses == virusesKilled
	isLoss = not isWin
	iw comb cond metric field record = (if cond then IM.insertWith comb startingViruses (LevelMetric metric fp) else id) (field record)

data BureaucracyGlobalState = BureaucracyGlobalState
	{ bgsLastGame :: Maybe FilePath
	, bgsNextTensor :: HashMap T.Text Integer
	, bgsMetadata :: HashMap T.Text CategoryMetadata
	} deriving (Eq, Ord, Read, Show)

newBureaucracyGlobalState :: BureaucracyGlobalState
newBureaucracyGlobalState = BureaucracyGlobalState
	{ bgsLastGame = Nothing
	, bgsNextTensor = HM.empty
	, bgsMetadata = HM.empty
	}

data BureaucracyThreadState = BureaucracyThreadState
	{ btsGamesProcessed :: HashMap T.Text Integer
	, btsTensorsProcessed :: HashMap T.Text Integer
	, btsRequestedSplit :: ValidationSplit
	, btsCurrentSplit :: ValidationSplit
	, btsLatestGlobal :: BureaucracyGlobalState
	} deriving Eq

newBureaucracyThreadState :: ValidationSplit -> BureaucracyThreadState
newBureaucracyThreadState vs = BureaucracyThreadState
	{ btsGamesProcessed = HM.empty
	, btsTensorsProcessed = HM.empty
	, btsRequestedSplit = vs
	, btsCurrentSplit = vs
	, btsLatestGlobal = newBureaucracyGlobalState
	}

btsRequestedSplitL :: BureaucracyThreadState -> (ValidationSplit, ValidationSplit -> BureaucracyThreadState)
btsRequestedSplitL bts = (btsRequestedSplit bts, \vs -> bts { btsRequestedSplit = vs })

btsUpdate :: TVar (Stable BureaucracyThreadState) -> (BureaucracyThreadState -> BureaucracyThreadState) -> IO ()
btsUpdate status = atomically . modifyTVar status . sTryUpdate

initialValidationSplit :: [(T.Text, Double)]
initialValidationSplit = [("train", 9), ("test", 1)]

-- ╭╴top╶────╮
-- │╭╴vsv╶──╮│
-- │╰───────╯│
-- │╭╴glg╶──╮│
-- │╰───────╯│
-- │╭╴int╶──╮│
-- ││╭╴glt╶╮││
-- ││╰─────╯││
-- ││╭╴tgp╶╮││
-- ││╰─────╯││
-- ││╭╴ttp╶╮││
-- ││╰─────╯││
-- │╰───────╯│
-- ╰─────────╯
bureaucracyThreadView :: Procedure LogMessage () -> MVar BureaucracyGlobalState -> IO ThreadView
bureaucracyThreadView log lock = do
	vs <- newValidationSplit initialValidationSplit
	burRef <- newTVarIO (newStable (newBureaucracyThreadState vs))
	tracker <- newTracker

	top <- new Box [#orientation := OrientationVertical, #spacing := 3]
	vsv <- newValidationSplitView vs $ atomically . modifyTVar burRef . sOnSubterm btsRequestedSplitL . sTrySet
	glg <- descriptionLabel "<initializing>\n"
	int <- new Grid []
	glt <- newSumView int 0 "tensors available to other threads"
	tgp <- newSumView int 1 "games processed by this thread"
	ttp <- newSumView int 2 "tensors processed by this thread"

	vsvWidget vsv >>= #append top
	#append top glg
	#append top int

	let refresh = do
	    	sbts <- readTVarIO burRef
	    	tWhenUpdated tracker sbts $ \bts -> do
	    		vsvSet vsv (btsCurrentSplit bts)
	    		set glg [#label := case bgsLastGame (btsLatestGlobal bts) of
	    			Nothing -> "no games processed yet\n"
	    			Just fp -> "latest game known to be processed was\n" <> T.pack fp
	    			]
	    		updateSumView glt (bgsNextTensor (btsLatestGlobal bts))
	    		updateSumView tgp (btsGamesProcessed bts)
	    		updateSumView ttp (btsTensorsProcessed bts)
	tvNew top refresh (bureaucracyThread log lock burRef)

bureaucracyThread :: Procedure LogMessage () -> MVar BureaucracyGlobalState -> TVar (Stable BureaucracyThreadState) -> StatusCheck -> IO a
bureaucracyThread log lock status sc = do
	dir <- nsDataDir
	forever $ do
		-- TODO: is this really doing the right thing if gameFileToTensorFiles throws an exception? seems like probably not?
		modifyMVar_ lock $ \bgs -> do
			pending <- catch (listDirectory (dir </> subdirectory GamesPending "")) $ \e ->
				if isDoesNotExistError e || isAlreadyInUseError e then pure [] else throw e
			btsUpdate status $ \bts -> bts { btsLatestGlobal = bgs }
			traverse_ (gameFileToTensorFiles log status dir) (sort pending)
			btsLatestGlobal . sPayload <$> readTVarIO status
		btsUpdate status $ \bts -> bts { btsCurrentSplit = btsRequestedSplit bts }
		-- TODO: comms from other threads to tell us when to try again
		threadDelay 1000000
		scIO_ sc

gameFileToTensorFiles :: Procedure LogMessage () -> TVar (Stable BureaucracyThreadState) -> FilePath -> FilePath -> IO ()
gameFileToTensorFiles log status dir fp = recallGame dir fp >>= \case
	GDStillWriting -> pure ()
	GDParseError -> do
		relocate dir fp GamesPending GamesParseError
		btsUpdate status $ \bts -> bts
			{ btsLatestGlobal = (btsLatestGlobal bts)
				{ bgsLastGame = Just fp
				}
			}
	GDSuccess history -> do
		bts <- sPayload <$> readTVarIO status
		categoryT <- vsSample (btsCurrentSplit bts)
		let category = dirEncode (T.unpack categoryT)
		    bgs = btsLatestGlobal bts
		firstTensor <- asum [empty
			, maybe empty pure $ HM.lookup categoryT (bgsNextTensor bgs)
			, maybe empty (pure . succ) =<< decodeFileLoop (dir </> subdirectory (Tensors category) latestFilename)
			, pure 0
			]
		meta_ <- asum [empty
			, maybe empty pure $ HM.lookup categoryT (bgsMetadata bgs)
			, maybe empty pure =<< decodeFileLoop (dir </> subdirectory (GamesProcessed category) metadataFilename)
			, pure newCategoryMetadata
			]

		tpath <- prepareFile dir (Tensors category) ""
		jspath <- prepareFile dir (Positions category) ""
		sts <- saveTensors tpath jspath firstTensor history
		let meta = cmInsert fp (stsVirusesOriginal sts) (stsVirusesKilled sts) (stsFrames sts) meta_

		relocate dir fp GamesPending (GamesProcessed category)
		encodeFileLoop
			(dir </> subdirectory (Tensors category) latestFilename)
			(firstTensor + stsTensorsSaved sts - 1)
		encodeFileLoop
			(dir </> subdirectory (GamesProcessed category) metadataFilename)
			meta

		let logKinds = zip ["viruses", "frames/won", "frames/lost" :: String]
		                   [cmVirusesKilled, cmFramesToWin, cmFramesToLoss]
		    logAggregations = zip ["best", "latest" :: String]
		                          [cmBest, cmLatest]
		    logAccumulations = zip3 ["viruses", "frames", "days" :: String]
		                            [cmVirusesKilled . cmCumulative, bothKindsOfFrames, bothKindsOfFrames]
		                            [1, 1, 60.0988*60*60*24]
		    bothKindsOfFrames cm = IM.unionWith lmSum (cmFramesToWin (cmCumulative cm)) (cmFramesToLoss (cmCumulative cm))

		traverse_ (schedule log)
			[ Metric (printf "%s/%s/%02d" k a (stsVirusesOriginal sts)) (lmDouble lm)
			| categoryT == "train"
			, (k, fk) <- logKinds
			, (a, fa) <- logAggregations
			, Just lm <- [IM.lookup (stsVirusesOriginal sts) (fk (fa meta))]
			]
		traverse_ (schedule log)
			[ Metric (printf "%s/%s/sum" k a) (sum (lmDouble <$> im))
			| categoryT == "train"
			, (k, fk) <- logKinds
			, (a, fa) <- logAggregations
			, let im = fk (fa meta)
			, IM.keys im == [4,8..84]
			]
		traverse_ (schedule log)
			[ Metric (printf "cumulative/%s" k) (lmDouble lm / denominator)
			| (k, f, denominator) <- logAccumulations
			, let lm = foldr (lmSum . foldr1 lmSum . f) (LevelMetric 0 "") (bgsMetadata (btsLatestGlobal bts))
			]

		btsUpdate status $ \bts -> bts
			{ btsLatestGlobal = BureaucracyGlobalState
				{ bgsLastGame = Just fp
				, bgsNextTensor = HM.insert categoryT (firstTensor + stsTensorsSaved sts) (bgsNextTensor (btsLatestGlobal bts))
				, bgsMetadata = HM.insert categoryT meta (bgsMetadata (btsLatestGlobal bts))
				}
			, btsGamesProcessed = HM.insertWith (+) categoryT 1 (btsGamesProcessed bts)
			, btsTensorsProcessed = HM.insertWith (+) categoryT (stsTensorsSaved sts) (btsTensorsProcessed bts)
			}

encodeFileLoop :: ToJSON a => FilePath -> a -> IO ()
encodeFileLoop fp a = catch
	(encodeFile fp a)
	(\e -> if isAlreadyInUseError e then threadDelay 1000 >> encodeFileLoop fp a else throwIO e)

decodeFileLoop :: FromJSON a => FilePath -> IO (Maybe a)
decodeFileLoop fp = catch (decodeFileStrict' fp) $ \e -> if
	| isAlreadyInUseError e -> threadDelay 1000 >> decodeFileLoop fp
	| isDoesNotExistError e -> pure Nothing
	| otherwise -> throwIO e

data GameDecodingResult
	= GDParseError
	| GDStillWriting
	| GDSuccess ((Board, Bool, CoarseSpeed), [GameStep])

recallGame :: FilePath -> FilePath -> IO GameDecodingResult
recallGame dir fp = handle (\e -> if isAlreadyInUseError e then pure GDStillWriting else throwIO e) $ do
	result <- decodeFileStrict' (dir </> subdirectory GamesPending fp)
	pure $ case result of
		Nothing -> GDParseError
		Just history -> GDSuccess history

data Directory
	= GamesPending
	| GamesProcessed FilePath
	| GamesParseError
	| Tensors FilePath
	| Positions FilePath
	| Weights
	deriving (Eq, Ord, Read, Show)

subdirectory :: Directory -> FilePath -> FilePath
subdirectory dir fp = case dir of
	GamesPending            -> "games" </> "pending" </> fp
	GamesProcessed category -> "games" </> "processed" </> category </> fp
	GamesParseError         -> "games" </> "parse-error" </> fp
	Tensors category        -> "tensors" </> category </> fp
	Positions category      -> "positions" </> category </> fp
	Weights                 -> "weights" </> fp

relocate :: FilePath -> FilePath -> Directory -> Directory -> IO ()
relocate root fp from to = do
	path <- prepareFile root to fp
	renameFile (root </> subdirectory from fp) path

prepareFile :: FilePath -> Directory -> FilePath -> IO FilePath
prepareFile root dir fp = (subdir </> fp) <$ createDirectoryIfMissing True subdir where
	subdir = root </> subdirectory dir ""

latestFilename, metadataFilename :: FilePath
latestFilename = "latest.json"
metadataFilename = "meta.json"

readLatestTensor :: FilePath -> FilePath -> IO (Maybe Integer)
readLatestTensor root category = decodeFileLoop
	(root </> subdirectory (Tensors category) latestFilename)

newSumView :: Grid -> Int32 -> T.Text -> IO Grid
newSumView parent i description = do
	lbl <- descriptionLabel description
	child <- new Grid [#columnSpacing := 7]
	#attach parent lbl   0 (2*i)   2 1
	#attach parent child 1 (2*i+1) 1 1
	pure child

updateSumView :: Grid -> HashMap T.Text Integer -> IO ()
updateSumView grid ns = do
	HM.traverseWithKey updateSumViewRow (enumerate ns)
	updateSumViewRow "total" (-1, totalI)
	where
	totalI = sum ns
	totalD = fromInteger totalI :: Double
	updateSumViewRow t (y_, n) = do
		gridUpdateLabelAt grid 0 y t (descriptionLabel t)
		gridUpdateLabelAt grid 1 y nt (numericLabel nt)
		gridUpdateLabelAt grid 2 y percent (numericLabel percent)
		where
		y = y_ + 1
		nt = tshow n
		percent = if totalI == 0
			then "100%"
			else tshow (round (100 * fromIntegral n / totalD)) <> "%"

data TrainingThreadState = TrainingThreadState
	{ ttsLastSaved :: Stable (Maybe Integer)
	, ttsCurrent :: Stable (Maybe Integer)
	, ttsGenerationHundredths :: SearchSpeed
	, ttsTensors :: SearchSpeed
	, ttsLoss :: Stable Double
	} deriving (Eq, Ord, Read, Show)

trainingThreadView :: Procedure LogMessage () -> TVar (Maybe Integer) -> IO ThreadView
trainingThreadView log netUpdate = do
	top <- new Box [#orientation := OrientationVertical]
	ogb <- new Box [#orientation := OrientationHorizontal]
	ogd <- descriptionLabel "current net: "
	ogv <- descriptionLabel "<still loading/sampling>"
	svb <- new Box [#orientation := OrientationHorizontal]
	svd <- descriptionLabel "most recently saved net: "
	svv <- descriptionLabel "<none yet>"
	lob <- new Box [#orientation := OrientationHorizontal]
	lod <- descriptionLabel "loss: "
	lov <- numericLabel "Infinity"
	spd <- new Grid []

	#append top ogb
	#append ogb ogd
	#append ogb ogv
	#append top svb
	#append svb svd
	#append svb svv
	#append top lob
	#append lob lod
	#append lob lov
	#append top spd

	ssGen0 <- newSearchSpeed
	ssTen0 <- newSearchSpeed
	ref <- newTVarIO TrainingThreadState
		{ ttsLastSaved = newStable Nothing
		, ttsCurrent = newStable Nothing
		, ttsGenerationHundredths = ssGen0
		, ttsTensors = ssTen0
		, ttsLoss = newStable (1/0)
		}
	tLastSaved <- newTracker
	tCurrent <- newTracker
	tLoss <- newTracker
	let refresh = do
	    	tts <- readTVarIO ref
	    	tWhenUpdated tLastSaved (ttsLastSaved tts) $ \case
	    		Nothing -> set svv [#label := "<none yet>"]
	    		Just ten -> set svv [#label := tshow ten]
	    	tWhenUpdated tCurrent (ttsCurrent tts) $ \case
	    		Nothing -> set ogv [#label := "<still loading/sampling>"]
	    		Just ten -> set ogv [#label := tshow ten]
	    	tWhenUpdated tLoss (ttsLoss tts) $ \loss -> set lov [#label := T.pack (printf "%7.3f" loss)]
	    	renderSpeeds spd [("epochs (%)", ttsGenerationHundredths tts), ("examples", ttsTensors tts)]

	tvNew top refresh (trainingThread log netUpdate ref)

-- TODO: do we have a stall condition to kill the AI if it survives too long without making progress?
-- TODO: make learning rate, batch size, and how many recent tensors to draw from configurable
-- TODO: perhaps instead of a flat recent tensor count, we should do exponential discounting!
-- TODO: might be nice to show current generation and iterations to next save
trainingThread :: Procedure LogMessage () -> TVar (Maybe Integer) -> TVar TrainingThreadState -> StatusCheck -> IO ()
trainingThread log netUpdate ref sc = do
	(ten0, net, sgd) <- trainingThreadLoadLatestNet
	atomically . modifyTVar ref $ \tts -> tts { ttsCurrent = sSet (Just ten0) (ttsCurrent tts) }
	rng <- createSystemRandom
	dir <- nsDataDir
	let loop ten = do
	    	-- this capitalization is inconsistent with the other metrics we
	    	-- report, but consistent with the other metrics shown in the
	    	-- System panel
	    	schedule log (Metric "System/Backprop Tensor Count" (fromInteger ten))
	    	when (ten `mod` tensorsPerSave < tensorsPerTrainI) (saveWeights ten)
	    	batch <- loadBatch rng sc dir "train" tensorHistoryTrain tensorsPerTrain
	    	-- TODO: make loss mask configurable
	    	loss <- netTrain net sgd batch fullLossMask
	    	schedule log (Metric "loss/train/sum" loss)
	    	when (ten `mod` tensorsPerDetailReport < tensorsPerTrainI) $ do
	    		testBatch <- loadBatch rng sc dir "test" tensorHistoryTest tensorsPerTest
	    		trainComponents <- netDetailedLoss net batch
	    		testComponents <- netDetailedLoss net testBatch
	    		schedule log $ Metric "loss/test/sum" (sum . map snd $ testComponents)
	    		traverse_ (schedule log)
	    			[ Metric ("loss/" <> category <> "/" <> describeLossType ty) loss
	    			| (category, components) <- [("train", trainComponents), ("test", testComponents)]
	    			, (ty, loss) <- components
	    			]
	    	when (ten `mod` tensorsPerVisualization < tensorsPerTrainI) $ do
	    		logVisualization log rng sc net dir "train" visualizationHistoryTrain
	    		logVisualization log rng sc net dir "test" visualizationHistoryTrain
	    	let ten' = ten+tensorsPerTrainI
	    	atomically . modifyTVar ref $ \tts -> tts
	    		{ ttsGenerationHundredths = ssIncBy (ttsGenerationHundredths tts) 100
	    		, ttsTensors = ssIncBy (ttsTensors tts) tensorsPerTrain
	    		, ttsCurrent = sSet (Just ten') (ttsCurrent tts)
	    		, ttsLoss = sSet loss (ttsLoss tts)
	    		}
	    	scIO sc (saveWeights ten')
	    	loop ten'

	    saveWeights ten = do
	    		path <- prepareFile dir Weights (show ten <.> "nsn")
	    		netSave net sgd path
	    		encodeFileLoop (dir </> subdirectory Weights latestFilename) ten
	    		atomically . writeTVar netUpdate $ Just ten
	    		atomically . modifyTVar ref $ \tts -> tts { ttsLastSaved = sSet (Just ten) (ttsLastSaved tts) }

	loop ten0
	where
	-- TODO: make these configurable
	tensorsPerSave = 100000
	tensorsPerDetailReport = 30000
	tensorsPerTrainI = toInteger tensorsPerTrain
	tensorsPerTest = 100
	tensorHistoryTrain = 500000
	tensorHistoryTest = 5000
	visualizationHistoryTrain = 1000
	tensorsPerVisualization = 1000000

tensorsPerTrain :: Int
tensorsPerTrain = 1200

trainingThreadLoadLatestNet :: IO (Integer, Net, Optimizer)
trainingThreadLoadLatestNet = do
	dir <- nsDataDir
	mn <- decodeFileLoop (dir </> subdirectory Weights latestFilename)
	case mn of
		Nothing -> do
			net <- netSample True
			sgd <- newOptimizer net
			pure (0, net, sgd)
		Just n -> do
			(net, sgd) <- netLoadForTraining (dir </> subdirectory Weights (show n <.> "nsn"))
			pure (n, net, sgd)

chooseTensors :: GenIO -> StatusCheck -> FilePath -> String -> Integer -> Int -> IO [Integer]
chooseTensors rng sc dir category historySize batchSize = do
	let go = readLatestTensor dir category >>= \case
	    	Nothing -> scIO_ sc >> threadDelay 1000000 >> go
	    	Just n -> pure n
	latestTensor <- go
	let earliestTensor = max 0 (latestTensor - historySize)
	replicateM batchSize (uniformRM (earliestTensor, latestTensor) rng)

loadBatch :: GenIO -> StatusCheck -> FilePath -> String -> Integer -> Int -> IO Batch
loadBatch rng sc dir category historySize batchSize = do
	ixs <- chooseTensors rng sc dir category historySize batchSize
	batchLoad [dir </> subdirectory (Tensors category) (show ix <.> "nst") | ix <- ixs]

logVisualization :: Procedure LogMessage () -> GenIO -> StatusCheck -> Net -> FilePath -> String -> Integer -> IO ()
logVisualization log rng sc net dir category historySize = do
	[ix] <- chooseTensors rng sc dir category historySize 1
	dataDir <- nsDataDir
	runtimeDir <- nsRuntimeDir
	netIn <- batchLoad [dir </> subdirectory (Tensors category) (show ix <.> "nst")]
	[netOut] <- netIntrospect net netIn
	mhst <- decodeFileLoop (dataDir </> subdirectory (Positions category) (show ix <.> "json"))
	now <- Time.getCurrentTime

	for_ mhst $ \hst -> do
		let (w, h) = heatmapSizeRecommendation (hstBoard hst)
		    scale = 16
		    [wi, hi] = (scale*) <$> [w, h]
		    [wd, hd] = fromIntegral <$> [w, h]
		    wb = width  (hstBoard hst)
		    hb = height (hstBoard hst)
		    initialPC = uncurry launchContent (hstLookahead hst)
		    rotatedPCs = iterate (`rotateContent` Clockwise) initialPC
		    pred = hstPrediction hst
		    allPCs = [PillContent or bl o | or <- [minBound..maxBound], bl <- [minBound..maxBound], o <- [minBound..maxBound]]
		    onPositions f = [(pos, f pos x y) | x <- [0..wb-1], y <- [0..hb-1], let pos = Position x y]
		    onPositionsVec f = onPositions (\_ x y -> f netOut V.! x V.! y)
		    onPositionsC f = onPositions (\pos _ _ -> realToFrac (f pos pred))
		    outPriors = zip3 [0..] rotatedPCs
		    	[ [ (Position x y, niPriors netOut V.! roti V.! x V.! y)
		    	  -- horizontal placements can't occur in the last column
		    	  | x <- [0..wb - 2 + (roti `rem` 2)], y <- [0..hb-1]
		    	  ]
		    	| roti <- [0..3]
		    	]
		    reachablePriors = [[p | p <- ps, Pill pc (fst p) `HM.member` hstPriors hst] | (_, pc, ps) <- outPriors]
		    reachablePriorsSum_ = sum (reachablePriors >>= map snd)
		    reachablePriorsSum = if reachablePriorsSum_ == 0 then 1 else reachablePriorsSum_
		    outPriorsLo = minimum [p | (_, _, ps) <- outPriors, (_, p) <- ps]
		    outPriorsHi = maximum [p | (_, _, ps) <- outPriors, (_, p) <- ps]
		    allPriorsHi = max
		    	(if null (hstPriors hst) then 0 else maximum (hstPriors hst)) -- TODO: scale hstPriors by 0.5 when bottomLeftColor == otherColor
		    	(maximum [p / reachablePriorsSum | ps <- reachablePriors, (_, p) <- ps])

		    logHeatmapGrid wg hg nm_ draw = G.withImageSurface G.FormatRGB24 (wg*wi) (hg*hi) $ \img -> do
		    	let nm = "visualization/" ++ category ++ "/" ++ nm_
		    	    dir = runtimeDir </> dirEncode nm
		    	    fp = dir </> show now ++ "-" ++ show ix <.> "png"
		    	G.renderWith img (initMath (wg*wi) (hg*hi) (fromIntegral wg*wd) (fromIntegral hg*hd) >> draw)
		    	createDirectoryIfMissing True dir
		    	G.surfaceWriteToPNG img fp
		    	schedule log (ImagePath nm fp)

		logHeatmapGrid 4 3 "priors" . for_ outPriors $ \(roti, pc, rotPriors) -> do
			G.save
			G.translate (fromIntegral roti*wd) 0
			heatmapRange outPriorsLo outPriorsHi (hstBoard hst) pc rotPriors
			G.translate 0 hd
			heatmap0Max allPriorsHi (hstBoard hst) pc
				[ (pos, p / reachablePriorsSum)
				| (pos, p) <- reachablePriors !! roti
				]
			G.translate 0 hd
			heatmap0Max allPriorsHi (hstBoard hst) pc
				[ (pos, prior)
				| (Pill pc' pos, prior) <- HM.toList (hstPriors hst)
				, pc' == pc
				]
			G.restore

		logHeatmapGrid 1 3 "virus kills" $ do
			let outKills = onPositionsVec niVirusKills
			heatmap0Dyn (hstBoard hst) initialPC outKills
			G.translate 0 hd
			heatmap01 (hstBoard hst) initialPC outKills
			G.translate 0 hd
			heatmap01 (hstBoard hst) initialPC . onPositionsC $ \pos -> M.findWithDefault 0 pos . pVirusKillWeight

		logHeatmapGrid 2 2 "non-heatmaps" $ do
			G.rectangle 0 0 20 42 >> neutral >> G.fill
			fitTexts $ tail [undefined
				, TextRequest { trX =  0  , trY =  0.4, trW = 8  , trH = 1.2, trText = "valuation" }
				, TextRequest { trX =  8.4, trY =  0.4, trW = 3.2, trH = 1.2, trText = printf "%.2e" (hstValuation hst) }
				, TextRequest { trX = 14.4, trY =  0.4, trW = 3.2, trH = 1.2, trText = printf "%.2e" (niValuation netOut) }
				, TextRequest { trX =  0  , trY =  2.4, trW = 8  , trH = 1.2, trText = "fall time" }
				, TextRequest { trX =  8.4, trY =  2.4, trW = 3.2, trH = 1.2, trText = show . toInteger $ pFallWeight pred }
				, TextRequest { trX = 14.4, trY =  2.4, trW = 3.2, trH = 1.2, trText = printf "%.2f" (niFallTime netOut) }
				, TextRequest { trX =  0  , trY = 40.4, trW = 8  , trH = 1.2, trText = "category" }
				, TextRequest { trX =  8.4, trY = 40.4, trW = 5.2, trH = 1.2, trText = "ground truth" }
				, TextRequest { trX = 14.4, trY = 40.4, trW = 5.2, trH = 1.2, trText = "net output" }
				]
			G.rectangle 12 0 2 2 >> bwGradient (hstValuation hst   ) >> G.fill
			G.rectangle 18 0 2 2 >> bwGradient ( niValuation netOut) >> G.fill
			forZipWithM_ [0..] allPCs $ \i pc -> do
				let blyI = 4 + 2*i; blyD = fromIntegral blyI
				    truth = realToFrac . HM.findWithDefault 0 pc $ pClearPillWeight pred
				    predicted = HM.findWithDefault 0 pc (niClearPill netOut)
				pill (Pill pc (Position 2 (blyI-1)))
				fitTexts $ tail [undefined
					, TextRequest { trX =  8.4, trY = blyD+0.4, trW = 3.2, trH = 1.2, trText = printf "%.2e" truth }
					, TextRequest { trX = 14.4, trY = blyD+0.4, trW = 3.2, trH = 1.2, trText = printf "%.2e" predicted }
					]
				G.rectangle 12 blyD 2 2 >> bwGradient truth     >> G.fill
				G.rectangle 18 blyD 2 2 >> bwGradient predicted >> G.fill

		logHeatmapGrid 1 3 "final occupation" $ do
			let outOccupied = onPositionsVec niOccupied
			heatmap0Dyn (hstBoard hst) initialPC outOccupied
			G.translate 0 hd
			heatmap01 (hstBoard hst) initialPC outOccupied
			G.translate 0 hd
			heatmap01 (hstBoard hst) initialPC . onPositionsC $ \pos p -> if HS.member pos (pOccupied p) then 1 else 0

		logHeatmapGrid (length allPCs) 2 "future placements" $ do
			let placementHi = max
			    	(realToFrac . maximum . (0:) . HM.elems $ pPlacementWeight pred)
			    	(maximum . fmap maximum . fmap (fmap maximum) $ niPlacements netOut)
			forZipWithM_ [0..] allPCs $ \i pc -> do
				G.save
				G.translate (i*wd) 0
				heatmap0Max placementHi (hstBoard hst) pc $ onPositionsVec ((HM.! pc) . niPlacements)
				G.translate 0 hd
				heatmap0Max placementHi (hstBoard hst) pc . onPositionsC $ \pos -> HM.findWithDefault 0 (Pill pc pos) . pPlacementWeight
				G.restore

		logHeatmapGrid 1 2 "clear locations" $ do
			let clearHi = max
			    	(realToFrac . maximum . (0:) . M.elems $ pClearLocationWeight pred)
			    	(maximum . fmap maximum $ niClearLocation netOut)
			heatmap0Max clearHi (hstBoard hst) initialPC (onPositionsVec niClearLocation)
			G.translate 0 hd
			heatmap0Max clearHi (hstBoard hst) initialPC . onPositionsC $ \pos -> M.findWithDefault 0 pos . pClearLocationWeight

data LogMessage
	= Metric String Double
	| ImagePath String FilePath
	deriving (Eq, Ord, Read, Show)

-- TODO: this really does not need to update its view 30 times per second, once per hour is enough
loggingThreadView :: Procedure LogMessage () -> String -> IO ThreadView
loggingThreadView log runName = do
	top <- new Box [#orientation := OrientationVertical]
	tvNew top (pure ()) (loggingThread log runName)

loggingThread :: Procedure LogMessage () -> String -> StatusCheck -> IO ()
loggingThread log runName sc = do
	wandbCat <- getDataFileName "pybits/wandb-cat.py"
	dir <- nsDataDir
	previousRuntime <- catch (readFile (dir </> "runtime") >>= readIO) \e ->
		if isDoesNotExistError e
			then pure 0
			else throwIO e
	createDirectoryIfMissing True (dir </> "wandb")
	(Just h, Nothing, Nothing, ph) <- createProcess (proc "python3" [wandbCat, "-q"]) { std_in = CreatePipe }

	hPutStrLn h dir
	hPutStrLn h "Nurse Sveta architecture experiments"
	hPutStrLn h runName
	hPutStrLn h "" -- no support for resuming (yet?)
	hPutStr h . unlines $ tail [undefined
		, "final layer", "convolutional" -- "fully connected"
		, "float size", "64" -- "32" "16"
		, "residual design", "batch normalization" -- "fixup"
		, "mixup", "no" -- "yes"
		, "residual blocks", "32" -- "doubling"
		, "epoch size", show tensorsPerTrain
		, "" -- the config section is null terminated lol
		]
	hFlush h

	start <- getTime Monotonic
	let go = \case
	    	Metric    k v -> reportMetric k (show v)
	    	ImagePath k v -> reportMetric k case v of
	    		'!':_ -> "!./" ++ v
	    		_     -> "!"   ++ v
	    reportMetric k s = do
	    	hPrint h =<< getRuntime
	    	hPutStrLn h k
	    	hPutStrLn h s
	    saveRuntime = writeFile (dir </> "runtime") . show =<< getRuntime
	    getRuntime = getTime Monotonic <&> \now -> previousRuntime + round (now - start)

	forever $ do
		step <- atomically . asum $ tail [undefined
			, Left <$> scSTM sc
			, Right <$> serviceCallsSTM_ log (traverse go)
			]
		case step of
			Left _ -> hClose h >> saveRuntime >> waitForProcess ph >> scIO_ sc
			Right act -> act >> saveRuntime

gridGetLabelAt :: Grid -> Int32 -> Int32 -> IO Label -> IO Label
gridGetLabelAt grid x y factory = #getChildAt grid x y >>= \case
	Just w -> castTo Label w >>= \case
		Just lbl -> pure lbl
		Nothing -> #remove grid w >> mkLabel
	Nothing -> mkLabel
	where
	mkLabel = do
		lbl <- factory
		#attach grid lbl x y 1 1
		pure lbl

gridUpdateLabelAt :: Grid -> Int32 -> Int32 -> T.Text -> IO Label -> IO ()
gridUpdateLabelAt grid x y t factory = do
	lbl <- gridGetLabelAt grid x y factory
	set lbl [#label := t]

descriptionLabel :: T.Text -> IO Label
descriptionLabel t = new Label [#label := t, #halign := AlignStart]

numericLabel :: T.Text -> IO Label
numericLabel t = do
	lbl <- new Label [#label := t, #justify := JustificationRight, #halign := AlignEnd, #cssClasses := ["mono"]]
	cssPrv <- new CssProvider []
	#loadFromData cssPrv ".mono { font-family: \"monospace\"; }"
	cssCtx <- #getStyleContext lbl
	#addProvider cssCtx cssPrv (fromIntegral STYLE_PROVIDER_PRIORITY_APPLICATION)
	pure lbl

dirEncode :: String -> FilePath
dirEncode "" = "q"
dirEncode s = go s where
	go = \case
		[] -> []
		'/' :s -> "qs" ++ go s
		'\\':s -> "qb" ++ go s
		'q' :s@(c:_) -> (if c `elem` ("sbq" :: String) then "qq" else "q") ++ go s
		"q" -> "qq"
		c:s -> c:go s

data SearchSpeed = SearchSpeed
	{ searchStart :: UTCTime
	, searchIterations :: Int
	} deriving (Eq, Ord, Read, Show)

newSearchSpeed :: IO SearchSpeed
newSearchSpeed = Time.getCurrentTime <&> \now -> SearchSpeed now 0

ssInc :: SearchSpeed -> SearchSpeed
ssInc ss = ss { searchIterations = searchIterations ss + 1 }

ssIncBy :: SearchSpeed -> Int -> SearchSpeed
ssIncBy ss n = ss { searchIterations = searchIterations ss + n }

data SearchSummary = SearchSummary
	{ rootPosition :: Stable PlayerStateModel
	, speeds :: [(T.Text, SearchSpeed)]
	} deriving (Eq, Ord, Read, Show)
