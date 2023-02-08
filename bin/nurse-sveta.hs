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
	inferenceProcedure <- newProcedure 100
	bureaucracyLock <- newMVar newBureaucracyGlobalState
	on app #activate $ do
		mainRef <- newIORef Nothing
		top <- new Box [#orientation := OrientationHorizontal, #spacing := 10]
		txt <- new Box [#orientation := OrientationVertical]

		gen <- newThreadManager "generation" Green (generationThreadView inferenceProcedure)
		inf <- newThreadManager "inference" OS (inferenceThreadView inferenceProcedure)
		bur <- newThreadManager "bureaucracy" Green (bureaucracyThreadView bureaucracyLock)
		trn <- newThreadManager "training" OS trainingThreadView
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
					-- Don't bother to cleanly shut down the inference thread.
					-- It doesn't have any resources worth being careful about,
					-- and stopping its threads can cause search threads to
					-- block instead of gracefully shutting down.
					let tms = [gen]
					writeIORef mainRef (Just (length tms))
					for_ tms $ \tm -> tmDieThen tm $ readIORef mainRef >>= \case
						Nothing -> fail "the impossible happened: a thread manager finished dying before thread managers started dying"
						Just 1 -> #quit app
						Just n -> writeIORef mainRef (Just (n-1))
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
		, speeds = HM.empty
		}
	, requestedConfiguration = cfg
	, currentConfiguration = newStable cfg
	}

onRootPosition :: (Stable PlayerStateModel -> Stable PlayerStateModel) -> GenerationThreadState -> GenerationThreadState
onRootPosition f gts = gts { summary = s { rootPosition = f (rootPosition s) } } where
	s = summary gts

onSpeeds :: (HashMap T.Text SearchSpeed -> HashMap T.Text SearchSpeed) -> GenerationThreadState -> GenerationThreadState
onSpeeds f gts = gts { summary = s { speeds = f (speeds s) } } where
	s = summary gts

initialSearchConfiguration :: SearchConfiguration
initialSearchConfiguration = SearchConfiguration
	{ c_puct = 1
	, iterations = 10000
	}

requestConfiguration :: SearchConfiguration -> GenerationThreadState -> GenerationThreadState
requestConfiguration sc gts = gts { requestedConfiguration = sc }

acceptConfiguration :: GenerationThreadState -> GenerationThreadState
acceptConfiguration gts = gts { currentConfiguration = sTrySetPayload (requestedConfiguration gts) (currentConfiguration gts) }

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
	genRef <- newTVarIO (newGenerationThreadState initialSearchConfiguration)

	psv <- newPlayerStateView (PSM (emptyBoard 8 16) Nothing [])
	scv <- newSearchConfigurationView initialSearchConfiguration (atomically . modifyTVar genRef . requestConfiguration)
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

renderSpeeds :: Grid -> HashMap T.Text SearchSpeed -> IO ()
renderSpeeds spd sss = do
	now <- Time.getCurrentTime
	let column nm ss = Ap . ZipList . map (:[]) $ [nm, ": ", tshow (searchIterations ss), " positions/", ms, "s = ", T.justifyRight 5 ' ' . tshow . round $ rate, " positions/s"] where
	    	dt = realToFrac . Time.diffUTCTime now . searchStart $ ss :: Double
	    	ms = T.pack (showFFloat (Just 1) (realToFrac dt) "")
	    	rate = fromIntegral (searchIterations ss) / dt
	    Ap (ZipList columns) = HM.foldMapWithKey column sss
	unless (HM.null sss) $ zipWithM_ updateLabel [0..] (T.unlines <$> columns)
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
		let params = dmParameters config eval
		(s0, t) <- initialTree params g
		s <- clone params s0
		gameSpeed <- newSearchSpeed
		moveLoop g config params threadSpeed gameSpeed s0 [] s t

	moveLoop g config params threadSpeed gameSpeed s0 history s t = do
		[l, r] <- map toEnum <$> replicateM 2 (uniformR (0, 2) g)
		t' <- unsafeDescend params (RNG l r) s t
		let gs = GameStep (RNG l r) mempty mempty
		boardSnapshot <- mfreeze (board s)
		atomically $ modifyTVar genRef (onRootPosition (sSetPayload (PSM boardSnapshot (Just (l, r)) [])))
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

			let [threadSpeed', gameSpeed', moveSpeed'] = incSearchIterations <$> [threadSpeed, gameSpeed, moveSpeed]
			    speeds' = HM.fromList [("thread", threadSpeed), ("game", gameSpeed), ("move", moveSpeed)]

			gts <- atomically . modifyTVar genRef $ id
				. onSpeeds (const speeds')
				. case move of
				  	Just (Placement _ p, _, _) -> onRootPosition (sOnSubterm psmOverlayL (sTrySetPayload [(p, 0.3)]))
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
	dir <- getUserDataDir "nurse-sveta"
	path <- prepareFile dir GamesPending $ show now ++ "-" ++ rand <.> "json"
	encodeFile path (b, reverse steps)

inferenceThreadView :: DMEvaluationProcedure -> IO ThreadView
inferenceThreadView eval = do
	spd <- new Grid []
	spdWidget <- toWidget spd
	infRef <- newTVarIO HM.empty
	pure ThreadView
		{ tvWidget = spdWidget
		, tvRefresh = readTVarIO infRef >>= renderSpeeds spd
		, tvCompute = inferenceThread eval infRef
		}

inferenceThread :: DMEvaluationProcedure -> TVar (HashMap T.Text SearchSpeed) -> StatusCheck -> IO ()
inferenceThread eval infRef sc = do
	net <- netSample False
	batches <- newSearchSpeed
	positions <- newSearchSpeed
	go net batches positions
	where
	go net batches positions = do
		atomically . writeTVar infRef $ HM.fromList [("batches", batches), ("positions", positions)]
		mn <- atomically
			$ (Just <$> serviceCallsSTM eval (\as -> flip (,) (length as) <$> netEvaluation net as))
			<|> (Nothing <$ scSTM sc)
		case mn of
			Just ion -> ion >>= \n -> go
				net
				(incSearchIterations batches)
				(positions { searchIterations = searchIterations positions + n })
			Nothing -> scIO sc

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
	vsv <- newValidationSplitView initialValidationSplit $ atomically . modifyTVar burRef . sOnSubterm btsRequestedSplitL . sTrySetPayload
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
	dir <- getUserDataDir $ "nurse-sveta"
	forever $ do
		-- TODO: is this really doing the right thing if gameFileToTensorFiles throws an exception? seems like probably not?
		modifyMVar_ lock $ \bgs -> do
			pending <- catch (listDirectory (dir </> subdirectory GamesPending "")) $ \e ->
				if isDoesNotExistError e then pure [] else throw e
			btsUpdate status $ \bts -> bts { btsLatestGlobal = bgs }
			traverse_ (gameFileToTensorFiles status dir) (sort pending)
			btsLatestGlobal . sPayload <$> readTVarIO status
		btsUpdate status $ \bts -> bts { btsCurrentSplit = btsRequestedSplit bts }
		-- TODO: comms from other threads to tell us when to try again
		threadDelay 1000000
		scIO sc

gameFileToTensorFiles :: TVar (Stable BureaucracyThreadState) -> FilePath -> FilePath -> IO ()
gameFileToTensorFiles status dir fp = recallGame dir fp >>= \case
	Nothing -> do
		relocate dir fp GamesPending GamesParseError
		btsUpdate status $ \bts -> bts
			{ btsLatestGlobal = (btsLatestGlobal bts)
				{ bgsLastGame = Just fp
				}
			}
	Just history -> do
		bts <- sPayload <$> readTVarIO status
		categoryT <- vsSample (btsCurrentSplit bts)
		let category = dirEncode (T.unpack categoryT)
		    bgs = btsLatestGlobal bts
		firstTensor <- asum [empty
			, maybe empty pure $ HM.lookup categoryT (bgsNextTensor bgs)
			, maybe empty (pure . succ) =<< decodeFileStrict' (dir </> subdirectory (Tensors category) latestTensorFilename)
			, pure 0
			]

		path <- prepareFile dir (Tensors category) ""
		n <- saveTensors path firstTensor history

		relocate dir fp GamesPending (GamesProcessed category)
		encodeFile (dir </> subdirectory (Tensors category) latestTensorFilename) (firstTensor + n - 1)
		btsUpdate status $ \bts -> bts
			{ btsLatestGlobal = BureaucracyGlobalState
				{ bgsLastGame = Just fp
				, bgsNextTensor = HM.insert categoryT (firstTensor + n) (bgsNextTensor (btsLatestGlobal bts))
				}
			, btsGamesProcessed = HM.insertWith (+) categoryT 1 (btsGamesProcessed bts)
			, btsTensorsProcessed = HM.insertWith (+) categoryT n (btsTensorsProcessed bts)
			}

recallGame :: FilePath -> FilePath -> IO (Maybe (Board, [GameStep]))
recallGame dir fp = decodeFileStrict' (dir </> subdirectory GamesPending fp)

data Directory
	= GamesPending
	| GamesProcessed FilePath
	| GamesParseError
	| Tensors FilePath
	deriving (Eq, Ord, Read, Show)

subdirectory :: Directory -> FilePath -> FilePath
subdirectory dir fp = case dir of
	GamesPending            -> "games" </> "pending" </> fp
	GamesProcessed category -> "games" </> "processed" </> category </> fp
	GamesParseError         -> "games" </> "parse-error"
	Tensors category        -> "tensors" </> category </> fp

relocate :: FilePath -> FilePath -> Directory -> Directory -> IO ()
relocate root fp from to = do
	path <- prepareFile root to fp
	renameFile (root </> subdirectory from fp) path

prepareFile :: FilePath -> Directory -> FilePath -> IO FilePath
prepareFile root dir fp = (subdir </> fp) <$ createDirectoryIfMissing True subdir where
	subdir = root </> subdirectory dir ""

latestTensorFilename :: FilePath
latestTensorFilename = "latest.json"

readLatestTensor :: FilePath -> FilePath -> IO (Maybe Integer)
readLatestTensor root category = catch
	(decodeFileStrict' (root </> subdirectory (Tensors category) latestTensorFilename))
	(\e -> if isDoesNotExistError e then pure Nothing else throwIO e)

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

trainingThreadView :: IO ThreadView
trainingThreadView = do
	top <- new Box [#orientation := OrientationVertical]
	lob <- new Box [#orientation := OrientationHorizontal]
	lod <- descriptionLabel "loss: "
	lov <- numericLabel "Infinity"
	spd <- new Grid []

	#append top lob
	#append lob lod
	#append lob lov
	#append top spd

	ss0 <- newSearchSpeed
	ref <- newTVarIO (ss0, 1/0)
	let refresh = do
	    	(ss, loss) <- readTVarIO ref
	    	set lov [#label := T.pack (printf "%7.3f" loss)]
	    	renderSpeeds spd (HM.singleton "iterations" ss)
	refresh

	topWidget <- toWidget top
	pure ThreadView
		{ tvWidget = topWidget
		, tvRefresh = refresh
		, tvCompute = trainingThread ref
		}

-- TODO: do we have a stall condition to kill the AI if it survives too long without making progress?
-- TODO: make learning rate, batch size, and how many recent tensors to draw from configurable
trainingThread :: TVar (SearchSpeed, Double) -> StatusCheck -> IO ()
trainingThread ref sc = do
	net <- netSample True
	sgd <- newOptimizer net
	gen <- createSystemRandom
	dir <- getUserDataDir $ "nurse-sveta"
	let readLatestTensorLoop = readLatestTensor dir category >>= \case
	    	Nothing -> scIO sc >> threadDelay 1000000 >> readLatestTensorLoop
	    	Just n -> pure n
	    loop i = do
	    	latestTensor <- readLatestTensorLoop
	    	let earliestTensor = max 0 (latestTensor - 5000)
	    	batchIndices <- replicateM 100 (uniformRM (earliestTensor, latestTensor) gen)
	    	batch <- batchLoad [dir </> subdirectory (Tensors category) (show ix <.> "nst") | ix <- batchIndices]
	    	loss <- netTrain net sgd batch
	    	atomically (modifyTVar ref (\(ss, _) -> (incSearchIterations ss, loss)))
	    	scIO sc
	    	loop (i+1)
	loop 0
	where
	category = "train"

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

incSearchIterations :: SearchSpeed -> SearchSpeed
incSearchIterations ss = ss { searchIterations = searchIterations ss + 1 }

data SearchSummary = SearchSummary
	{ rootPosition :: Stable PlayerStateModel
	, speeds :: HashMap T.Text SearchSpeed
	} deriving (Eq, Ord, Read, Show)
