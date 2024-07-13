module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Encoding
import Data.Foldable
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.IntMap (IntMap)
import Data.IORef
import Data.List
import Data.Maybe
import Data.Sequence (Seq)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Dr.Mario.Model
import GI.Gtk as G
import Immutable.Shuffle
import Numeric
import Nurse.Sveta.Files
import Nurse.Sveta.STM
import Nurse.Sveta.STM.BatchProcessor
import Nurse.Sveta.Tomcats
import Nurse.Sveta.Torch
import Nurse.Sveta.Util
import Nurse.Sveta.Widget
import System.Clock.Seconds
import System.Environment
import System.IO
import System.IO.Error
import System.Mem
import System.Process
import System.Random.MWC
import Text.Printf
import Util

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.Internal.Encoding.Utf8 as T
import qualified Data.Time as Time
import qualified Data.Vector as V

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
		log <- newThreadManager "logging" Green (loggingThreadView loggingProcedure)
		replicateM_ 15 (tmStartThread gen)
		tmStartThread inf
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
	args <- getArgs
	() <$ #run app (Just args)

commaSeparatedNumber :: (Integral a, Show a) => a -> T.Text
commaSeparatedNumber = T.pack . flip go "" where
	go n = case n `quotRem` 1000 of
		(0, _) -> shows n
		(q, r) -> go q . (',':) . (pad (show r) ++)
	pad s = replicate (3-length s) '0' ++ s

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
	, iterations = 200
	, typicalMoves = 25
	, priorNoise = 0.1
	, temperature = 2
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
generationThreadView :: Procedure NetInput NetOutput -> IO ThreadView
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
	let row (nm, ss) = [nm, ": ", commaSeparatedNumber (searchIterations ss), " positions/", ms, "s = ", T.justifyRight 5 ' ' . tshow . precision 10 $ rate, " positions/s"] where
	    	dt = realToFrac . Time.diffUTCTime now . searchStart $ ss :: Double
	    	ms = T.pack (showFFloat (Just 1) (realToFrac dt) "")
	    	rate = fromIntegral (searchIterations ss) / dt
	    	precision prec n = fromInteger (round (n*prec)) / prec
	    columns = transpose (map row sss)
	unless (null sss) $ zipWithM_ updateLabel [0..] (T.unlines <$> columns)
	where
	updateLabel n t = gridUpdateLabelAt spd n 0 t (numericLabel t)

generationThread :: Procedure NetInput NetOutput -> TVar GenerationThreadState -> StatusCheck -> IO ()
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
		atomically $ modifyTVar genRef (onRootPosition (sSet (PSM boardSnapshot (Just (Lookahead l r)) [])))
		moveSpeed <- newSearchSpeed
		searchLoop g config params s0 (gs:history) s threadSpeed gameSpeed moveSpeed t' (iterations config)

	searchLoop g config params s0 history s = innerLoop where
		innerLoop threadSpeed gameSpeed moveSpeed t 0 = descend g config params s t >>= \case
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
			-- Sometimes we'll produce trees that have visit counts lower than
			-- the number of iterations we've done. See the comment inside
			-- dmExpand for more on why.
			--
			-- This whole setup is very messy. At some point we should really
			-- revisit the mcts design from the ground up and try to make
			-- something a bit cleaner.
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
	-- The clever version uses BS.foldr (printf "%02x%s"). The mundane version
	-- below is definitely linear-time in the length of the bytestring, because
	-- ++ doesn't do a deep copy of its second argument. It's not so clear in
	-- the clever version; I think it's possible each printf would do a deep
	-- copy, leading to quadratic time.
	rand <- BS.foldr (\w s -> printf "%02x" w ++ s) "" <$> withFile "/dev/urandom" ReadMode (\h -> BS.hGet h 8)
	path <- relPrepareFile GamesPending $ show now ++ "-" ++ rand <.> "json"
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

inferenceThreadView :: Procedure NetInput NetOutput -> TVar (Maybe Integer) -> IO ThreadView
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
		Just (n, _) -> commaSeparatedNumber n

data InferenceThreadStep
	= ITSLoadNet (Maybe Integer)
	| ITSProgress (IO Int)
	| ITSDie

inferenceThread :: Procedure NetInput NetOutput -> TVar (Maybe Integer) -> TVar InferenceThreadState -> StatusCheck -> IO ()
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
	liftEvaluation :: (V.Vector NetInput -> IO (V.Vector NetOutput)) -> STM InferenceThreadStep
	liftEvaluation f = ITSProgress <$> serviceCallsSTM eval (fmap (\answers -> (answers, V.length answers)) . f)

inferenceThreadLoadLatestNet :: IO (Maybe (Integer, Net))
inferenceThreadLoadLatestNet = do
	n <- relDecodeFileLoop Weights latestFilename
	traverse inferenceThreadLoadNet n

-- TODO: better error handling
inferenceThreadLoadNet :: Integer -> IO (Integer, Net)
inferenceThreadLoadNet tensor = do
	dir <- nsDataDir
	-- [N]urse [S]veta [n]et
	net <- netLoadForInference (absFileName dir Weights (show tensor <.> "nsn"))
	pure (tensor, net)

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

lmNeither :: LevelMetric -> LevelMetric -> LevelMetric
lmNeither _lm _lm' = LevelMetric
	{ lmMetric = -1
	, lmSource = "<none>"
	}

lmFloat :: LevelMetric -> Float
lmFloat = fromIntegral . lmMetric

data CategoryMetrics = CategoryMetrics
	{ cmVirusesKilled :: IntMap LevelMetric
	, cmFramesToWin   :: IntMap LevelMetric
	, cmFramesToLoss  :: IntMap LevelMetric
	, cmFrames        :: IntMap LevelMetric
	} deriving (Eq, Ord, Read, Show)

vkFieldName, ftwFieldName, ftlFieldName :: Key
vkFieldName = "viruses killed"
ftwFieldName = "frames to win"
ftlFieldName = "frames to loss"
frFieldName = "frames"

instance ToJSON CategoryMetrics where
	toJSON cm = object $ tail [undefined
		,  vkFieldName .= cmVirusesKilled cm
		, ftwFieldName .= cmFramesToWin cm
		, ftlFieldName .= cmFramesToLoss cm
		,  frFieldName .= cmFrames cm
		]
	toEncoding cm = pairs $ mempty
		<>  vkFieldName .= cmVirusesKilled cm
		<> ftwFieldName .= cmFramesToWin cm
		<> ftlFieldName .= cmFramesToLoss cm
		<>  frFieldName .= cmFrames cm

instance FromJSON CategoryMetrics where
	parseJSON = withObject "CategoryMetrics" $ \v -> pure CategoryMetrics
		<*> v .:  vkFieldName
		<*> v .: ftwFieldName
		<*> v .: ftlFieldName
		<*> v .:  frFieldName

newCategoryMetrics :: CategoryMetrics
newCategoryMetrics = CategoryMetrics
	{ cmVirusesKilled = mempty
	, cmFramesToWin   = mempty
	, cmFramesToLoss  = mempty
	, cmFrames        = mempty
	}

-- invariant: corresponding fields in cmBest and cmLatest have the same keys
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
	{ cmBest       = mkMetrics lmMost lmFewest lmMost lmNeither b
	, cmLatest     = mkMetrics const  const    const  const     r
	, cmCumulative = mkMetrics lmSum  lmSum    lmSum  lmSum     c
	} where
	mkMetrics vk fw fl fr cm = CategoryMetrics
		{ cmVirusesKilled = iw vk True   virusesKilled cmVirusesKilled cm
		, cmFramesToWin   = iw fw isWin  frames        cmFramesToWin   cm
		, cmFramesToLoss  = iw fl isLoss frames        cmFramesToLoss  cm
		, cmFrames        = iw fr True   frames        cmFrames        cm
		}
	isWin = startingViruses == virusesKilled
	isLoss = not isWin
	iw comb cond metric field record = (if cond then IM.insertWith comb startingViruses (LevelMetric metric fp) else id) (field record)

data BureaucracyGlobalState = BureaucracyGlobalState
	{ bgsLastGame :: Maybe FilePath
	, bgsGameNames :: HashMap T.Text (Seq T.Text)
	, bgsMetadata :: HashMap T.Text CategoryMetadata
	} deriving (Eq, Ord, Read, Show)

newBureaucracyGlobalState :: BureaucracyGlobalState
newBureaucracyGlobalState = BureaucracyGlobalState
	{ bgsLastGame = Nothing
	, bgsGameNames = HM.empty
	, bgsMetadata = HM.empty
	}

data BureaucracyThreadState = BureaucracyThreadState
	{ btsGamesProcessed :: HashMap T.Text Integer
	, btsPillsProcessed :: HashMap T.Text Integer
	, btsRequestedSplit :: ValidationSplit
	, btsCurrentSplit :: ValidationSplit
	, btsLatestGlobal :: BureaucracyGlobalState
	} deriving Eq

newBureaucracyThreadState :: ValidationSplit -> BureaucracyThreadState
newBureaucracyThreadState vs = BureaucracyThreadState
	{ btsGamesProcessed = HM.empty
	, btsPillsProcessed = HM.empty
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
-- ││╭╴tpp╶╮││
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
	glt <- newSumView int 0 "games available to other threads"
	tgp <- newSumView int 1 "games processed by this thread"
	tpp <- newSumView int 2 "pills processed by this thread"

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
	    		updateSumView glt (toInteger . S.length <$> bgsGameNames (btsLatestGlobal bts))
	    		updateSumView tgp (btsGamesProcessed bts)
	    		updateSumView tpp (btsPillsProcessed bts)
	tvNew top refresh (bureaucracyThread log lock burRef)

bureaucracyThread :: Procedure LogMessage () -> MVar BureaucracyGlobalState -> TVar (Stable BureaucracyThreadState) -> StatusCheck -> IO a
bureaucracyThread log lock status sc = do
	dir <- nsDataDir
	forever $ do
		-- TODO: is this really doing the right thing if gameFileToTensorFiles throws an exception? seems like probably not?
		modifyMVar_ lock $ \bgs -> do
			pending <- catch (listDirectory (absDirectoryName dir GamesPending)) $ \e ->
				if isDoesNotExistError e || isAlreadyInUseError e then pure [] else throw e
			btsUpdate status $ \bts -> bts { btsLatestGlobal = bgs }
			traverse_ (processGameFile log status dir) (sort pending)
			btsLatestGlobal . sPayload <$> readTVarIO status
		btsUpdate status $ \bts -> bts { btsCurrentSplit = btsRequestedSplit bts }
		-- TODO: comms from other threads to tell us when to try again
		threadDelay 1000000
		scIO_ sc

processGameFile :: Procedure LogMessage () -> TVar (Stable BureaucracyThreadState) -> FilePath -> FilePath -> IO ()
processGameFile log status dir fp = recallGame dir fp >>= \case
	GDStillWriting -> pure ()
	GDParseError -> do
		relocate dir fp GamesPending GamesParseError
		btsUpdate status $ \bts -> bts
			{ btsLatestGlobal = (btsLatestGlobal bts)
				{ bgsLastGame = Just fp
				}
			}
	GDSuccess (seed, steps) -> do
		bts <- sPayload <$> readTVarIO status
		categoryT <- vsSample (btsCurrentSplit bts)
		let categoryS = T.unpack categoryT
		    category = dirEncode categoryS
		    bgs = btsLatestGlobal bts
		gameNames_ <- asum [empty
			, maybe empty pure $ HM.lookup categoryT (bgsGameNames bgs)
			, maybe empty pure =<< absDecodeFileLoop dir (GamesProcessed category) namesFilename
			, pure S.empty
			]
		meta_ <- asum [empty
			, maybe empty pure $ HM.lookup categoryT (bgsMetadata bgs)
			, maybe empty pure =<< absDecodeFileLoop dir (GamesProcessed category) metadataFilename
			, pure newCategoryMetadata
			]

		gs <- initialState seed
		traverse_ (dmPlay gs . gsMove) steps
		kills <- readIORef (virusesKilled gs)
		pills <- readIORef (pillsUsed gs)
		frames <- readIORef (framesPassed gs)
		let viruses = originalVirusCount gs
		    meta = cmInsert fp viruses kills frames meta_
		    -- TODO: make 10000 configurable
		    gameNames = S.take 10000 (gameNames_ S.:|> T.pack fp)
		    processedDir = absDirectoryName dir (GamesProcessed category)
		    tmpNames = processedDir </> ('.':namesFilename)

		relocate dir fp GamesPending (GamesProcessed category)
		rawEncodeFileLoop (processedDir </> metadataFilename) meta
		-- other threads may be reading this file as we write it, so we need to
		-- be a bit careful about atomicity
		rawEncodeFileLoop tmpNames gameNames
		renameFile tmpNames (processedDir </> namesFilename)

		let logKinds = zip ["viruses", "frames/won", "frames/lost" :: String]
		                   [cmVirusesKilled, cmFramesToWin, cmFramesToLoss]
		    logAggregations = zip ["best", "latest" :: String]
		                          [cmBest, cmLatest]
		    logAccumulations = zip3 ["viruses", "frames", "days" :: String]
		                            [cmVirusesKilled, cmFrames, cmFrames]
		                            [1, 1, fps*60*60*24]
		    fps = 60.0988

		traverse_ (schedule log)
			[ Metric (printf "%s/%s/%02d" k a viruses) (lmFloat lm)
			| categoryT == "train"
			, (k, fk) <- logKinds
			, (a, fa) <- logAggregations
			, Just lm <- [IM.lookup viruses (fk (fa meta))]
			]
		traverse_ (schedule log)
			[ Metric (printf "%s/%s/sum" k a) (sum (lmFloat <$> im))
			| categoryT == "train"
			, (k, fk) <- logKinds
			, (a, fa) <- logAggregations
			, let im = fk (fa meta)
			, IM.keys im == [4,8..84]
			]
		traverse_ (schedule log)
			[ Metric (printf "cumulative/%s" k) (lmFloat lm / denominator)
			| (k, f, denominator) <- logAccumulations
			, let lm = foldr (lmSum . foldr1 lmSum . f . cmCumulative) (LevelMetric 0 "") (bgsMetadata (btsLatestGlobal bts))
			]
		traverse_ (schedule log)
			[ Metric ("clear rate/" ++ k) (fromIntegral f / (fps * fromIntegral v))
			| (k, f, v) <- zip3
				[printf "%02d" viruses, "avg/" ++ categoryS]
				[frames, sum (lmMetric <$> cmFrames        (cmLatest meta))]
				[kills , sum (lmMetric <$> cmVirusesKilled (cmLatest meta))]
			, v /= 0
			]

		btsUpdate status $ \bts -> bts
			{ btsLatestGlobal = BureaucracyGlobalState
				{ bgsLastGame = Just fp
				, bgsGameNames = HM.insert categoryT gameNames (bgsGameNames (btsLatestGlobal bts))
				, bgsMetadata = HM.insert categoryT meta (bgsMetadata (btsLatestGlobal bts))
				}
			, btsGamesProcessed = HM.insertWith (+) categoryT 1 (btsGamesProcessed bts)
			, btsPillsProcessed = HM.insertWith (+) categoryT (toInteger pills) (btsPillsProcessed bts)
			}

data GameDecodingResult
	= GDParseError
	| GDStillWriting
	| GDSuccess GameDetails

recallGame :: FilePath -> FilePath -> IO GameDecodingResult
recallGame dir fp = handle (\e -> if isAlreadyInUseError e then pure GDStillWriting else throwIO e) $ do
	result <- decodeFileStrict' (absFileName dir GamesPending fp)
	pure $ case result of
		Nothing -> GDParseError
		Just history -> GDSuccess history

readGameNames :: FilePath -> FilePath -> IO (Maybe (Seq T.Text))
readGameNames root category = absDecodeFileLoop root (GamesProcessed category) namesFilename

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
		nt = commaSeparatedNumber n
		percent = if totalI == 0
			then "100%"
			else tshow (round (100 * fromIntegral n / totalD)) <> "%"

data TrainingConfiguration = TrainingConfiguration
	{ tcDutyCycle :: Stable Double
	, tcHoursPerSave :: Double
	, tcHoursPerDetailReport :: Double
	, tcBatchSizeTrain :: Int
	, tcBatchSizeTest :: Int
	, tcLossScaling :: LossScaling
	} deriving (Eq, Ord, Read, Show)

newTrainingConfiguration :: TrainingConfiguration
newTrainingConfiguration = TrainingConfiguration
	{ tcDutyCycle = newStable 0.01
	, tcHoursPerSave = 1
	, tcHoursPerDetailReport = 0.1
	, tcBatchSizeTrain = 2000 -- TODO: optimize this choice (empirically, maxing out GPU memory is not the fastest choice)
	, tcBatchSizeTest = 100
	-- TODO: make this configurable
	, tcLossScaling = LossScaling
		{ lsPriors = 0.1
		, lsValuation = 100
		}
	}

data TrainingThreadState = TrainingThreadState
	{ ttsLastSaved :: Stable (Maybe Integer)
	, ttsCurrent :: Stable (Maybe Integer)
	, ttsGenerationHundredths :: SearchSpeed
	, ttsTensors :: SearchSpeed
	, ttsDutyCycle :: SearchSpeed
	, ttsLoss :: Stable Float
	, ttsRequestedConfiguration :: TrainingConfiguration
	, ttsCurrentConfiguration :: Stable TrainingConfiguration
	} deriving (Eq, Ord, Read, Show)

ttsRequest :: TVar TrainingThreadState -> (TrainingConfiguration -> a -> Maybe TrainingConfiguration) -> a -> IO Bool
ttsRequest ref f a = atomically $ do
	tts <- readTVar ref
	case f (ttsRequestedConfiguration tts) a of
		Nothing -> pure False
		Just tc -> True <$ writeTVar ref tts { ttsRequestedConfiguration = tc }

ttsAccept :: TrainingThreadState -> TrainingThreadState
ttsAccept tts = tts { ttsCurrentConfiguration = sTrySet (ttsRequestedConfiguration tts) (ttsCurrentConfiguration tts) }

-- ╭╴top╶───────────╮
-- │╭╴ogb╶─────────╮│
-- ││╭╴ogd╶╮╭╴ogv╶╮││
-- ││╰─────╯╰─────╯││
-- │╰──────────────╯│
-- │╭╴svb╶─────────╮│
-- ││╭╴svd╶╮╭╴svv╶╮││
-- ││╰─────╯╰─────╯││
-- │╰──────────────╯│
-- │╭╴lob╶─────────╮│
-- ││╭╴lod╶╮╭╴lov╶╮││
-- ││╰─────╯╰─────╯││
-- │╰──────────────╯│
-- │╭╴spd╶╮         │
-- │╰─────╯         │
-- │╭╴cfg╶──╮       │
-- ││╭╴dut╶╮│       │
-- ││╰─────╯│       │
-- ││╭╴hps╶╮│       │
-- ││╰─────╯│       │
-- ││╭╴hpd╶╮│       │
-- ││╰─────╯│       │
-- ││╭╴bsr╶╮│       │
-- ││╰─────╯│       │
-- ││╭╴bse╶╮│       │
-- ││╰─────╯│       │
-- │╰───────╯       │
-- ╰────────────────╯
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
	cfg <- new Grid crvGridAttributes

	ssGen0 <- newSearchSpeed
	ssTen0 <- newSearchSpeed
	ssDut0 <- newSearchSpeed
	ref <- newTVarIO TrainingThreadState
		{ ttsLastSaved = newStable Nothing
		, ttsCurrent = newStable Nothing
		, ttsGenerationHundredths = ssGen0
		, ttsTensors = ssTen0
		, ttsDutyCycle = ssDut0
		, ttsLoss = newStable (1/0)
		, ttsRequestedConfiguration = newTrainingConfiguration
		, ttsCurrentConfiguration = newStable newTrainingConfiguration
		}

	let newCfg :: (Eq a, Read a, Show a) => (TrainingConfiguration -> a) -> T.Text -> InputPurpose -> (TrainingConfiguration -> a -> Maybe TrainingConfiguration) -> IO (ConfigurationRequestView a)
	    newPosCfg :: (Ord a, Num a, Read a, Show a) => (TrainingConfiguration -> a) -> T.Text -> (TrainingConfiguration -> a -> TrainingConfiguration) -> IO (ConfigurationRequestView a)
	    newCfg field nm purpose set = newConfigurationRequestView (field newTrainingConfiguration) nm "next batch" purpose (ttsRequest ref set)
	    newPosCfg field nm set = newCfg field nm InputPurposeDigits $ \tc n -> n > 0 ? set tc n
	dut <- newCfg (sPayload . tcDutyCycle) "duty cycle" InputPurposeNumber $ \tc dutyCycle -> 0 <= dutyCycle && dutyCycle <= 1 ? tc { tcDutyCycle = sTrySet dutyCycle (tcDutyCycle tc) }
	hps <- newPosCfg tcHoursPerSave          "hours per save"            $ \tc n -> tc { tcHoursPerSave          = n }
	hpd <- newPosCfg tcHoursPerDetailReport  "hours per detailed report" $ \tc n -> tc { tcHoursPerDetailReport  = n }
	bsr <- newPosCfg tcBatchSizeTrain        "training batch size"       $ \tc n -> tc { tcBatchSizeTrain        = n }
	bse <- newPosCfg tcBatchSizeTest         "test batch size"           $ \tc n -> tc { tcBatchSizeTest         = n }

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
	#append top cfg
	crvAttach dut cfg 0
	crvAttach hps cfg 1
	crvAttach hpd cfg 2
	crvAttach bsr cfg 3
	crvAttach bse cfg 4

	tLastSaved <- newTracker
	tCurrent <- newTracker
	tLoss <- newTracker
	tCfg <- newTracker
	let refresh = do
	    	tts <- readTVarIO ref
	    	tWhenUpdated tLastSaved (ttsLastSaved tts) $ \case
	    		Nothing -> set svv [#label := "<none yet>"]
	    		Just ten -> set svv [#label := commaSeparatedNumber ten]
	    	tWhenUpdated tCurrent (ttsCurrent tts) $ \case
	    		Nothing -> set ogv [#label := "<still loading/sampling>"]
	    		Just ten -> set ogv [#label := commaSeparatedNumber ten]
	    	tWhenUpdated tLoss (ttsLoss tts) $ \loss -> set lov [#label := T.pack (printf "%7.3f" loss)]
	    	let dc = sPayload . tcDutyCycle . sPayload . ttsCurrentConfiguration $ tts
	    	tWhenUpdated tCfg (ttsCurrentConfiguration tts) $ \cfg -> do
	    		crvSet dut dc
	    		crvSet hps (tcHoursPerSave          cfg)
	    		crvSet hpd (tcHoursPerDetailReport  cfg)
	    		crvSet bsr (tcBatchSizeTrain        cfg)
	    		crvSet bse (tcBatchSizeTest         cfg)
	    	renderSpeeds spd [("batches (%)", ttsGenerationHundredths tts), ("thread", ttsTensors tts), ("at " <> tshow dc, ttsDutyCycle tts)]

	tvNew top refresh (trainingThread log netUpdate ref)

-- TODO: do we have a stall condition to kill the AI if it survives too long without making progress?
-- TODO: make learning rate, batch size, and how many recent tensors to draw from configurable
-- TODO: perhaps instead of a flat recent tensor count, we should do exponential discounting!
-- TODO: might be nice to show current generation and iterations to next save
trainingThread :: Procedure LogMessage () -> TVar (Maybe Integer) -> TVar TrainingThreadState -> StatusCheck -> IO ()
trainingThread log netUpdate ref sc = do
	(ten0, (net, sgd)) <- trainingThreadLoadLatestNet
	rng <- createSystemRandom
	dir <- nsDataDir

	threadStart <- Time.getCurrentTime
	saveT <- newIORef threadStart
	detailT <- newIORef threadStart
	visualizationT <- newIORef threadStart
	tDutyCycle <- newTracker

	let loop ten = do
	    	cfg <- atomically . stateTVar ref $ \tts -> (ttsRequestedConfiguration tts, ttsAccept tts)
	    	tWhenUpdated_ tDutyCycle (tcDutyCycle cfg) $ do
	    		ss <- newSearchSpeed
	    		atomically . modifyTVar ref $ \tts -> tts { ttsDutyCycle = ss }

	    	-- this capitalization is inconsistent with the other metrics we
	    	-- report, but consistent with the other metrics shown in the
	    	-- System panel
	    	schedule log (Metric "System/Backprop Tensor Count" (fromInteger ten))
	    	every (tcHoursPerSave cfg) saveT (saveWeights ten)
	    	batch <- loadBatch rng sc dir "train" (tcBatchSizeTrain cfg)
	    	before <- Time.getCurrentTime
	    	loss <- netTrain net sgd (tcLossScaling cfg) batch
	    	after <- Time.getCurrentTime
	    	schedule log (Metric "loss/train/sum" loss)

	    	every (tcHoursPerDetailReport cfg) detailT $ do
	    		testBatch <- loadBatch rng sc dir "test" (tcBatchSizeTest cfg)
	    		trainComponents <- netLossComponents net (tcLossScaling cfg) batch
	    		testComponents <- netLossComponents net (tcLossScaling cfg) testBatch
	    		schedule log $ Metric "loss/test/sum" (sum . map snd $ testComponents)
	    		traverse_ (schedule log)
	    			[ Metric (intercalate "/" ("loss":category:ty)) loss
	    			| (category, components) <- [("train", trainComponents), ("test", testComponents)]
	    			, (ty, loss) <- components
	    			]

	    	let ten' = ten+toInteger (tcBatchSizeTrain cfg)
	    	atomically . modifyTVar ref $ \tts -> tts
	    		{ ttsGenerationHundredths = ssIncBy (ttsGenerationHundredths tts) 100
	    		, ttsTensors = ssIncBy (ttsTensors tts) (tcBatchSizeTrain cfg)
	    		, ttsDutyCycle = ssIncBy (ttsDutyCycle tts) (tcBatchSizeTrain cfg)
	    		, ttsCurrent = sSet (Just ten') (ttsCurrent tts)
	    		, ttsLoss = sSet loss (ttsLoss tts)
	    		}

	    	-- set up a timeout to pause (for the duty cycle)
	    	timeoutRef <- newTVarIO False
	    	timeoutID <- forkIO $ case sPayload (tcDutyCycle cfg) of
	    		0 -> pure ()
	    		d -> let t = realToFrac (Time.diffUTCTime after before) in do
	    			threadDelay (round (1000000*t*(1-d)/d))
	    			atomically (writeTVar timeoutRef True)

	    	-- die if it's been requested; otherwise wait until the end of the
	    	-- duty cycle or a new duty cycle has been requested, whichever
	    	-- comes first
	    	join . atomically . asum $ tail [undefined
	    		, scIO sc (saveWeights ten') <$ scSTM sc
	    		, do
	    		  	True <- readTVar timeoutRef
	    		  	pure (loop ten')
	    		, do
	    		  	tts <- readTVar ref
	    		  	when (tcDutyCycle (ttsRequestedConfiguration tts) == tcDutyCycle cfg) retry
	    		  	pure (loop ten')
	    		]

	    saveWeights ten = do
	    		path <- absPrepareFile dir Weights (show ten <.> "nsn")
	    		netSave net sgd path
	    		absEncodeFileLoop dir Weights latestFilename ten
	    		atomically . writeTVar netUpdate $ Just ten
	    		atomically . modifyTVar ref $ \tts -> tts { ttsLastSaved = sSet (Just ten) (ttsLastSaved tts) }

	-- make sure the weights are on disk for other threads to load
	when (ten0 == 0) (saveWeights ten0)
	atomically . modifyTVar ref $ \tts -> tts { ttsCurrent = sSet (Just ten0) (ttsCurrent tts) }
	loop ten0
	where
	every hours tref act = do
		prev <- readIORef tref
		now <- Time.getCurrentTime
		when (Time.diffUTCTime now prev > realToFrac (60*60*hours)) $ do
			act
			writeIORef tref now

trainingThreadLoadLatestNet :: IO (Integer, (Net, Optimizer))
trainingThreadLoadLatestNet = do
	dir <- nsDataDir
	mn <- absDecodeFileLoop dir Weights latestFilename
	case mn of
		Nothing -> do
			netOptim <- netSample
			pure (0, netOptim)
		Just n -> do
			netOptim <- netLoadForTraining (absFileName dir Weights (show n <.> "nsn"))
			pure (n, netOptim)

-- This doesn't choose training positions uniformly at random from among all
-- those available (it's biased towards positions from longer games), but
-- hopefully it's close enough anyway.
loadBatch :: GenIO -> StatusCheck -> FilePath -> String -> Int -> IO (Vector TrainingExample)
loadBatch rng sc dir category batchSize = do
	(numGames, gameNames) <- gameNamesLoop
	let gameLoadLoop n = if n < batchSize
	    	then do
	    		i <- uniformRM (0, numGames-1) rng
	    		mgd <- absDecodeFileLoop dir (GamesProcessed category) (T.unpack (S.index gameNames i))
	    		tes <- fromMaybe V.empty <$> traverse trainingExamples mgd
	    		(tes:) <$> gameLoadLoop (n + V.length tes)
	    	else pure []
	sampleWithoutReplacement batchSize . V.concat =<< gameLoadLoop 0
	where
	gameNamesLoop = readGameNames dir category >>= \case
		Just nms | len > 0 -> pure (len, nms) where len = S.length nms
		_ -> scIO_ sc >> threadDelay 1000000 >> gameNamesLoop

data LogMessage
	= Metric String Float
	| ImagePath String FilePath
	deriving (Eq, Ord, Read, Show)

-- TODO: this really does not need to update its view 30 times per second, once per hour is enough
loggingThreadView :: Procedure LogMessage () -> IO ThreadView
loggingThreadView log = do
	top <- new Box [#orientation := OrientationVertical]
	tvNew top (pure ()) (loggingThread log)

loggingThread :: Procedure LogMessage () -> StatusCheck -> IO ()
loggingThread log sc = do
	wandbCat <- getDataFileName "pybits/wandb-cat.py"
	dir <- nsDataDir
	previousRuntime <- catch (readFile (dir </> runtimeFilename) >>= readIO) \e ->
		if isDoesNotExistError e
			then pure 0
			else throwIO e
	absPrepareDirectory dir Logging
	(Just h, Nothing, Nothing, ph) <- createProcess (proc "python3" [wandbCat, "-q"]) { std_in = CreatePipe }

	hPutStrLn h dir
	hPutStrLn h "Nurse Sveta"
	Time.getCurrentTime >>= hPrint h
	hPutStrLn h "" -- no support for resuming (yet?)
	hPutStrLn h "" -- no reporting of configuration data (yet?)
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
	    saveRuntime = writeFile (dir </> runtimeFilename) . show =<< getRuntime
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
	loadFromDataLen cssPrv ".mono { font-family: \"monospace\"; }"
	cssCtx <- #getStyleContext lbl
	#addProvider cssCtx cssPrv (fromIntegral STYLE_PROVIDER_PRIORITY_APPLICATION)
	pure lbl

loadFromDataLen :: CssProvider -> T.Text -> IO ()
loadFromDataLen cssPrv t = #loadFromData cssPrv t tLen where
	tLen = fromIntegral $ T.foldl' (\len c -> len + T.utf8Length c) 0 t

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
