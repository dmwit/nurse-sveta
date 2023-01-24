module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Fixed
import Data.Foldable
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Time (UTCTime)
import Dr.Mario.Model
import Dr.Mario.STM
import Dr.Mario.STM.BatchProcessor
import Dr.Mario.Tomcats
import Dr.Mario.Torch
import Dr.Mario.Widget
import GI.Gtk as G
import Numeric
import System.Directory
import System.Environment
import System.Environment.XDG.BaseDir
import System.FilePath
import System.IO
import System.Random.MWC
import Text.Printf

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Time as Time

-- ╭╴w╶───────────────────────────╮
-- │╭╴top╶───────────────────────╮│
-- ││╭╴gen╶╮╭╴inf╶╮╭╴trn╶╮╭╴rep╶╮││
-- ││╰─────╯╰─────╯╰─────╯╰─────╯││
-- │╰────────────────────────────╯│
-- ╰──────────────────────────────╯
main :: IO ()
main = do
	-- On my machine, torch and gtk fight over the GPU. This environment
	-- variable setting instructs gtk not to do hardware acceleration --
	-- letting torch win the fight.
	lookupEnv "GSK_RENDERER" >>= \case
		Nothing -> setEnv "GSK_RENDERER" "cairo"
		_ -> pure ()

	app <- new Application []
	inferenceProcedure <- newProcedure 100
	on app #activate $ do
		mainRef <- newIORef Nothing
		top <- new Box [#orientation := OrientationHorizontal, #spacing := 10]

		gen <- newThreadManager "generation" Green (generationThreadView inferenceProcedure)
		inf <- newThreadManager "inference" OS (inferenceThreadView inferenceProcedure)
		replicateM_ 3 (tmStartThread gen)
		tmStartThread inf
		tmWidget gen >>= #append top
		tmWidget inf >>= #append top

		w <- new Window $ tail [undefined
			, #title := "Nurse Sveta"
			, #application := app
			, #child := top
			, #defaultWidth := 1000
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
	updateLabel n t = #getChildAt spd n 0 >>= \case
		Just w -> castTo Label w >>= \case
			Just lbl -> set lbl [#label := t]
			Nothing -> #remove spd w >> mkLabel n t
		Nothing -> mkLabel n t
	mkLabel n t = do
		lbl <- new Label [#label := t, #justify := JustificationRight, #cssClasses := ["mono"]]
		cssPrv <- new CssProvider []
		#loadFromData cssPrv ".mono { font-family: \"monospace\"; }"
		cssCtx <- #getStyleContext lbl
		#addProvider cssCtx cssPrv (fromIntegral STYLE_PROVIDER_PRIORITY_APPLICATION)
		#attach spd lbl n 0 1 1

data GameStep = GameStep
	{ gsMove :: Move
	, gsRoot :: Statistics
	, gsChildren :: HashMap Move Statistics
	} deriving (Eq, Ord, Read, Show)

instance ToJSON GameStep where toJSON gs = toJSON (gsMove gs, gsRoot gs, HM.toList (gsChildren gs))
instance FromJSON GameStep where parseJSON v = parseJSON v <&> \(m, r, c) -> GameStep m r (HM.fromList c)

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
			Just (m, t') -> moveLoop g config params threadSpeed gameSpeed s0 (gs:history) s t' where
				gs = GameStep m (statistics t) (statistics <$> children t)

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
	let date = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d" now
	    time = Time.formatTime Time.defaultTimeLocale "%T.%q" now
	    file = printf "%s-%s.json" time rand
	dir <- getUserDataDir $ "nurse-sveta" </> date
	createDirectoryIfMissing True dir
	encodeFile (dir </> file) (b, reverse steps)

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
