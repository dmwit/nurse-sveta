module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Fixed
import Data.Foldable
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Time (UTCTime)
import Dr.Mario.Model
import GI.Gtk as G
import Numeric
import Nurse.Sveta.STM
import Nurse.Sveta.STM.BatchProcessor
import Nurse.Sveta.Tomcats
import Nurse.Sveta.Torch
import Nurse.Sveta.Util
import Nurse.Sveta.Widget
import System.Directory
import System.Environment
-- TODO: this is now built into the directory package, we should switch to that
import System.Environment.XDG.BaseDir
import System.FilePath
import System.IO
import System.IO.Error
import System.Mem
import System.Random.MWC
import Text.Printf
import Util

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Time as Time

-- ╭╴w╶──────────────────────╮
-- │╭╴top╶──────────────────╮│
-- ││╭╴gen╶╮╭╴txt╶──╮╭╴rep╶╮││
-- ││╰─────╯│╭╴inf╶╮│╰─────╯││
-- ││       │╰─────╯│       ││
-- ││       │╭╴bur╶╮│       ││
-- ││       │╰─────╯│       ││
-- ││       │╭╴trn╶╮│       ││
-- ││       │╰─────╯│       ││
-- ││       ╰───────╯       ││
-- │╰───────────────────────╯│
-- ╰─────────────────────────╯
main :: IO ()
main = do
	torchPlusGtkFix
	app <- new Application []
	on app #activate $ do
		inferenceProcedure <- newProcedure 100
		bureaucracyLock <- newMVar newBureaucracyGlobalState
		netUpdate <- newTVarIO Nothing
		mainRef <- newIORef Nothing

		top <- new Box [#orientation := OrientationHorizontal, #spacing := 10]
		txt <- new Box [#orientation := OrientationVertical]

		gen <- newThreadManager "generation" Green (generationThreadView inferenceProcedure)
		inf <- newThreadManager "inference" OS (inferenceThreadView inferenceProcedure netUpdate)
		bur <- newThreadManager "bureaucracy" Green (bureaucracyThreadView bureaucracyLock)
		trn <- newThreadManager "training" OS (trainingThreadView netUpdate)
		replicateM_ 3 (tmStartThread gen)
		tmStartThread inf
		tmStartThread bur
		tmStartThread trn
		tmWidget gen >>= #append top
		tmWidget inf >>= #append txt
		tmWidget bur >>= #append txt
		tmWidget trn >>= #append txt
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
					    	-- performGC to run finalizers
					    	Just 1 -> performGC >> #quit app
					    	Just n -> writeIORef mainRef (Just (n-1))
					    tms = [bur, trn] -- gen, inf handled specially (see below)
					writeIORef mainRef (Just (length tms + 2))
					for_ tms $ \tm -> tm `tmDieThen` quitIfAppropriate
					-- don't kill off the inference threads until the
					-- generation threads are done, otherwise they might block
					-- waiting for a reply
					gen `tmDieThen` do
						inf `tmDieThen` quitIfAppropriate
						quitIfAppropriate
					pure True
			]
		#show w
	args <- getArgs
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
	{ c_puct = 1
	, iterations = 2000
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
generationThreadView :: DMEvaluationProcedure -> IO ThreadView
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

	topWidget <- toWidget top
	pure ThreadView
		{ tvWidget = topWidget
		, tvRefresh = do
			gts <- readTVarIO genRef
			renderSpeeds spd (speeds (summary gts))
			tWhenUpdated psvTracker (rootPosition (summary gts)) (psvSet psv)
			tWhenUpdated scvTracker (currentConfiguration gts) (scvSet scv)
		, tvCompute = generationThread eval genRef
		}

renderSpeeds :: Grid -> [(T.Text, SearchSpeed)] -> IO ()
renderSpeeds spd sss = do
	now <- Time.getCurrentTime
	let row (nm, ss) = [nm, ": ", tshow (searchIterations ss), " positions/", ms, "s = ", T.justifyRight 5 ' ' . tshow . round $ rate, " positions/s"] where
	    	dt = realToFrac . Time.diffUTCTime now . searchStart $ ss :: Double
	    	ms = T.pack (showFFloat (Just 1) (realToFrac dt) "")
	    	rate = fromIntegral (searchIterations ss) / dt
	    columns = transpose (map row sss)
	unless (null sss) $ zipWithM_ updateLabel [0..] (T.unlines <$> columns)
	where
	updateLabel n t = gridUpdateLabelAt spd n 0 t (numericLabel t)

generationThread :: DMEvaluationProcedure -> TVar GenerationThreadState -> StatusCheck -> IO ()
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
			let move = maximumOn (\_ -> visitCount . statistics) (children t')
			-- if mcts has thrown an error somewhere that matters, force it
			-- before we get into the critical section
			case move of
				Just (Placement _ p, _, _) -> p `seq` pure ()
				_ -> pure ()

			let [threadSpeed', gameSpeed', moveSpeed'] = ssInc <$> [threadSpeed, gameSpeed, moveSpeed]
			    speeds' = [("thread", threadSpeed), ("game", gameSpeed), ("move", moveSpeed)]

			gts <- atomically . modifyTVar genRef $ id
				. onSpeeds (const speeds')
				. case move of
				  	Just (Placement _ p, _, _) -> onRootPosition (sOnSubterm psmOverlayL (sTrySet [(p, 0.3)]))
				  	_ -> id

			scIO sc
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
	encodeFile path (b, reverse steps)

data InferenceThreadState = InferenceThreadState
	{ itsThreadBatches :: SearchSpeed
	, itsThreadPositions :: SearchSpeed
	, itsNetBatches :: SearchSpeed
	, itsNetPositions :: SearchSpeed
	, itsNet :: Stable (Maybe (Integer, Net))
	}

itsEvaluate :: Traversable t => InferenceThreadState -> t (GameState, Color, Color) -> IO (t DetailedEvaluation, Int)
itsEvaluate its queries = (\answers -> (answers, length answers)) <$> case sPayload (itsNet its) of
	Nothing -> traverse dumbEvaluation queries
	Just (_, net) -> netEvaluation net queries

itsNewNet :: InferenceThreadState -> Maybe Integer -> Bool
itsNewNet its mn = fmap fst (sPayload (itsNet its)) /= mn

inferenceThreadView :: DMEvaluationProcedure -> TVar (Maybe Integer) -> IO ThreadView
inferenceThreadView eval netUpdate = do
	top <- new Box [#orientation := OrientationVertical]
	lbl <- descriptionLabel "<initializing net>"
	spd <- new Grid []
	#append top lbl
	#append top spd

	latestNet <- inferenceThreadLoadLatestNet
	atomically $ do
		updatedNet <- readTVar netUpdate
		case (latestNet, updatedNet) of
			(Just (n, _), Nothing) -> writeTVar netUpdate (Just n)
			_ -> pure ()
	its0 <- pure InferenceThreadState
		<*> newSearchSpeed
		<*> newSearchSpeed
		<*> newSearchSpeed
		<*> newSearchSpeed
		<*> pure (newStable latestNet)
	ref <- newTVarIO its0

	tracker <- newTracker
	topWidget <- toWidget top
	pure ThreadView
		{ tvWidget = topWidget
		, tvRefresh = do
			its <- readTVarIO ref
			tWhenUpdated tracker (itsNet its) $ \mn ->
				set lbl [#label := describeNet mn]
			renderSpeeds spd $ tail [undefined
				, ("positions (thread)", itsThreadPositions its)
				, ("positions (net)   ", itsNetPositions its)
				, ("batches (thread)", itsThreadBatches its)
				, ("batches (net)   ", itsNetBatches its)
				]
		, tvCompute = inferenceThread eval netUpdate ref
		}
	where
	describeNet mn = "current net: " <> case mn of
		Nothing -> "hand-crafted"
		Just (n, _) -> tshow n

data InferenceThreadStep
	= ITSLoadNet (Maybe Integer)
	| ITSProgress (IO Int)
	| ITSDie

inferenceThread :: DMEvaluationProcedure -> TVar (Maybe Integer) -> TVar InferenceThreadState -> StatusCheck -> IO ()
inferenceThread eval netUpdate itsRef sc = forever $ do
	its <- readTVarIO itsRef
	step <- atomically . asum $ tail [undefined
		, ITSProgress <$> serviceCallsSTM eval (itsEvaluate its)
		, ITSDie <$ scSTM sc
		, ITSLoadNet <$> (readTVar netUpdate >>= ensure (itsNewNet its))
		]
	case step of
		ITSProgress ion -> ion >>= \n -> atomically $ writeTVar itsRef its
			{ itsThreadBatches = ssInc (itsThreadBatches its)
			, itsNetBatches = ssInc (itsNetBatches its)
			, itsThreadPositions = ssIncBy (itsThreadPositions its) n
			, itsNetPositions = ssIncBy (itsNetPositions its) n
			}
		ITSLoadNet n -> do
			net <- traverse inferenceThreadLoadNet n
			when (itsNewNet its (fst <$> net)) $ do
				batches <- newSearchSpeed
				positions <- newSearchSpeed
				atomically $ writeTVar itsRef its { itsNet = sSet net (itsNet its), itsNetBatches = batches, itsNetPositions = positions }
		ITSDie -> scIO sc

inferenceThreadLoadLatestNet :: IO (Maybe (Integer, Net))
inferenceThreadLoadLatestNet = do
	dir <- nsDataDir
	n <- catch
		(decodeFileStrict' (dir </> subdirectory Weights latestFilename))
		(\e -> if isDoesNotExistError e then pure Nothing else throwIO e)
	traverse inferenceThreadLoadNet n

-- TODO: better error handling
inferenceThreadLoadNet :: Integer -> IO (Integer, Net)
inferenceThreadLoadNet generation = do
	dir <- nsDataDir
	-- [N]urse [S]veta [n]et
	net <- netLoadForInference (dir </> subdirectory Weights (show generation <.> "nsn"))
	pure (generation, net)

nsDataDir :: IO FilePath
nsDataDir = getUserDataDir "nurse-sveta"

data BureaucracyGlobalState = BureaucracyGlobalState
	{ bgsLastGame :: Maybe FilePath
	, bgsNextTensor :: HashMap T.Text Integer
	} deriving (Eq, Ord, Read, Show)

newBureaucracyGlobalState :: BureaucracyGlobalState
newBureaucracyGlobalState = BureaucracyGlobalState
	{ bgsLastGame = Nothing
	, bgsNextTensor = HM.empty
	}

data BureaucracyThreadState = BureaucracyThreadState
	{ btsGamesProcessed :: HashMap T.Text Integer
	, btsTensorsProcessed :: HashMap T.Text Integer
	, btsRequestedSplit :: ValidationSplit
	, btsCurrentSplit :: ValidationSplit
	, btsLatestGlobal :: BureaucracyGlobalState
	} deriving (Eq, Ord, Read, Show)

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

initialValidationSplit :: ValidationSplit
initialValidationSplit = newValidationSplit [("train", 8), ("test", 1), ("validate", 1)]

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
bureaucracyThreadView :: MVar BureaucracyGlobalState -> IO ThreadView
bureaucracyThreadView lock = do
	burRef <- newTVarIO (newStable (newBureaucracyThreadState initialValidationSplit))
	tracker <- newTracker

	top <- new Box [#orientation := OrientationVertical, #spacing := 3]
	vsv <- newValidationSplitView initialValidationSplit $ atomically . modifyTVar burRef . sOnSubterm btsRequestedSplitL . sTrySet
	glg <- descriptionLabel "<initializing>\n"
	int <- new Grid []
	glt <- newSumView int 0 "tensors available to other threads"
	tgp <- newSumView int 1 "games processed by this thread"
	ttp <- newSumView int 2 "tensors processed by this thread"

	vsvWidget vsv >>= #append top
	#append top glg
	#append top int

	topWidget <- toWidget top
	pure ThreadView
		{ tvWidget = topWidget
		, tvRefresh = do
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
		, tvCompute = bureaucracyThread lock burRef
		}

bureaucracyThread :: MVar BureaucracyGlobalState -> TVar (Stable BureaucracyThreadState) -> StatusCheck -> IO a
bureaucracyThread lock status sc = do
	dir <- nsDataDir
	forever $ do
		-- TODO: is this really doing the right thing if gameFileToTensorFiles throws an exception? seems like probably not?
		modifyMVar_ lock $ \bgs -> do
			pending <- catch (listDirectory (dir </> subdirectory GamesPending "")) $ \e ->
				if isDoesNotExistError e || isAlreadyInUseError e then pure [] else throw e
			btsUpdate status $ \bts -> bts { btsLatestGlobal = bgs }
			traverse_ (gameFileToTensorFiles status dir) (sort pending)
			btsLatestGlobal . sPayload <$> readTVarIO status
		btsUpdate status $ \bts -> bts { btsCurrentSplit = btsRequestedSplit bts }
		-- TODO: comms from other threads to tell us when to try again
		threadDelay 1000000
		scIO sc

gameFileToTensorFiles :: TVar (Stable BureaucracyThreadState) -> FilePath -> FilePath -> IO ()
gameFileToTensorFiles status dir fp = recallGame dir fp >>= \case
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
			, maybe empty (pure . succ) =<< decodeFileStrict' (dir </> subdirectory (Tensors category) latestFilename)
			, pure 0
			]

		path <- prepareFile dir (Tensors category) ""
		n <- saveTensors path firstTensor history

		relocate dir fp GamesPending (GamesProcessed category)
		encodeFile (dir </> subdirectory (Tensors category) latestFilename) (firstTensor + n - 1)
		btsUpdate status $ \bts -> bts
			{ btsLatestGlobal = BureaucracyGlobalState
				{ bgsLastGame = Just fp
				, bgsNextTensor = HM.insert categoryT (firstTensor + n) (bgsNextTensor (btsLatestGlobal bts))
				}
			, btsGamesProcessed = HM.insertWith (+) categoryT 1 (btsGamesProcessed bts)
			, btsTensorsProcessed = HM.insertWith (+) categoryT n (btsTensorsProcessed bts)
			}

data GameDecodingResult
	= GDParseError
	| GDStillWriting
	| GDSuccess (Board, [GameStep])

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
	| Weights
	deriving (Eq, Ord, Read, Show)

subdirectory :: Directory -> FilePath -> FilePath
subdirectory dir fp = case dir of
	GamesPending            -> "games" </> "pending" </> fp
	GamesProcessed category -> "games" </> "processed" </> category </> fp
	GamesParseError         -> "games" </> "parse-error" </> fp
	Tensors category        -> "tensors" </> category </> fp
	Weights                 -> "weights" </> fp

relocate :: FilePath -> FilePath -> Directory -> Directory -> IO ()
relocate root fp from to = do
	path <- prepareFile root to fp
	renameFile (root </> subdirectory from fp) path

prepareFile :: FilePath -> Directory -> FilePath -> IO FilePath
prepareFile root dir fp = (subdir </> fp) <$ createDirectoryIfMissing True subdir where
	subdir = root </> subdirectory dir ""

latestFilename :: FilePath
latestFilename = "latest.json"

readLatestTensor :: FilePath -> FilePath -> IO (Maybe Integer)
readLatestTensor root category = catch
	(decodeFileStrict' (root </> subdirectory (Tensors category) latestFilename))
	(\e -> if isDoesNotExistError e || isAlreadyInUseError e then pure Nothing else throwIO e)

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

trainingThreadView :: TVar (Maybe Integer) -> IO ThreadView
trainingThreadView netUpdate = do
	top <- new Box [#orientation := OrientationVertical]
	svb <- new Box [#orientation := OrientationHorizontal]
	svd <- descriptionLabel "most recently saved net: "
	svv <- descriptionLabel "<none yet>"
	lob <- new Box [#orientation := OrientationHorizontal]
	lod <- descriptionLabel "loss: "
	lov <- numericLabel "Infinity"
	spd <- new Grid []

	#append top svb
	#append svb svd
	#append svb svv
	#append top lob
	#append lob lod
	#append lob lov
	#append top spd

	ss0 <- newSearchSpeed
	ref <- newTVarIO (newStable Nothing, ss0, 1/0)
	tracker <- newTracker
	let refresh = do
	    	(sgen, ss, loss) <- readTVarIO ref
	    	tWhenUpdated tracker sgen $ \case
	    		Nothing -> set svv [#label := "<none yet>"]
	    		Just gen -> set svv [#label := tshow gen]
	    	set lov [#label := T.pack (printf "%7.3f" loss)]
	    	renderSpeeds spd [("iterations", ss)]
	refresh

	topWidget <- toWidget top
	pure ThreadView
		{ tvWidget = topWidget
		, tvRefresh = refresh
		, tvCompute = trainingThread netUpdate ref
		}

-- TODO: do we have a stall condition to kill the AI if it survives too long without making progress?
-- TODO: make learning rate, batch size, and how many recent tensors to draw from configurable
-- TODO: perhaps instead of a flat recent tensor count, we should do exponential discounting!
-- TODO: might be nice to show current generation and iterations to next save
trainingThread :: TVar (Maybe Integer) -> TVar (Stable (Maybe Integer), SearchSpeed, Double) -> StatusCheck -> IO ()
trainingThread netUpdate ref sc = do
	(gen0, net, sgd) <- trainingThreadLoadLatestNet
	rng <- createSystemRandom
	dir <- nsDataDir
	let readLatestTensorLoop = readLatestTensor dir category >>= \case
	    	Nothing -> scIO sc >> threadDelay 1000000 >> readLatestTensorLoop
	    	Just n -> pure n
	    loop i gen = do
	    	i' <- if i >= 10000
	    		then do
	    			path <- prepareFile dir Weights (show gen <.> "nsn")
	    			netSave net sgd path
	    			encodeFile (dir </> subdirectory Weights latestFilename) gen
	    			atomically $ writeTVar netUpdate (Just gen)
	    			atomically . modifyTVar ref $ \(sgen, ss, loss) -> (sSet (Just gen) sgen, ss, loss)
	    			pure 1
	    		else pure (i+1)
	    	latestTensor <- readLatestTensorLoop
	    	let earliestTensor = max 0 (latestTensor - 5000)
	    	batchIndices <- replicateM 100 (uniformRM (earliestTensor, latestTensor) rng)
	    	batch <- batchLoad [dir </> subdirectory (Tensors category) (show ix <.> "nst") | ix <- batchIndices]
	    	loss <- netTrain net sgd batch
	    	atomically (modifyTVar ref (\(lastSavedGen, ss, _) -> (lastSavedGen, ssInc ss, loss)))
	    	scIO sc -- TODO: do one last netSave
	    	loop i' (gen+1)
	loop 1 gen0
	where
	category = "train"

trainingThreadLoadLatestNet :: IO (Integer, Net, Optimizer)
trainingThreadLoadLatestNet = do
	dir <- nsDataDir
	mn <- catch
		(decodeFileStrict' (dir </> subdirectory Weights latestFilename))
		(\e -> if isDoesNotExistError e then pure Nothing else throwIO e)
	case mn of
		Nothing -> do
			net <- netSample True
			sgd <- newOptimizer net
			pure (1, net, sgd)
		Just n -> do
			(net, sgd) <- netLoadForTraining (dir </> subdirectory Weights (show n <.> "nsn"))
			pure (n, net, sgd)

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
	lbl <- new Label [#label := t, #justify := JustificationRight, #cssClasses := ["mono"]]
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
