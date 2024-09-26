module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Aeson
import Data.Bits
import Data.Char
import Data.HashMap.Strict (HashMap)
import Data.IORef
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
import Nurse.Sveta.Widget
import System.Environment
import System.Random.MWC
import System.Mem
import Util

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

-- ╭╴w╶─────────────╮
-- │╭╴top╶─────────╮│
-- ││╭╴ply╶╮╭╴gen╶╮││
-- ││╰─────╯╰─────╯││
-- │╰──────────────╯│
-- ╰────────────────╯
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
		ply <- newThreadManager "evaluation" Green (evaluationThreadView mmc jobs)
		gen <- newThreadManager "evolution" Green (evolutionThreadView mmc jobs)
		#append top =<< tmWidget ply
		#append top =<< tmWidget gen
		replicateM_ (mmcInitialEvaluationThreads mmc) (tmStartThread ply)
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
					True <$ ply `tmDieThen` (performGC >> #quit app)
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
evaluationThread mmc jobs psmRef sc = forever do
	scIO_ sc
	job <- takeMVar jobs
	let gs = jGame job
	    moveLoop [] = moveLoop (jLookaheads job)
	    moveLoop (lk:lks) = finished gs >>= \b -> if b then pure () else do
	    	scIO_ sc
	    	cur <- mfreeze (board gs)
	    	atomically $ writeTVar psmRef PSM { psmBoard = cur, psmLookahead = Just lk, psmOverlay = [] }
	    	fp <- readIORef (framesPassed gs)
	    	pu <- readIORef (pillsUsed gs)
	    	placements <- mapproxReachable (board gs) (fp .&. 1 /= fromEnum (originalSensitive gs)) (gravity (speed gs) pu)
	    	let moves = V.fromList . HM.toList . HM.fromListWith shorterPath $
	    	    	[(mpPill placement lk, path) | (placement, path) <- HM.toList placements]
	    	next <- for moves \(pill, path) -> do
	    		gs' <- cloneGameState gs
	    		playMove gs' path pill
	    		mfreeze (board gs')
	    	let bestScore = V.maxIndex $ gEvaluate (jGenome job) cur next
	    	    (pill, path) = moves V.! bestScore
	    	playMove gs path pill
	    	moveLoop lks
	moveLoop []
	vk <- readIORef (virusesKilled gs)
	putMVar (jReply job) Evaluation
		{ eID = jID job
		, eViruses = vk
		, eFramesToLastKill = error "not yet implemented"
		}

data GenerationOverview = GenerationOverview
	{ goID :: Int
	, goPopulationSize :: Int
	, goPopulationEvaluated :: Int
	} deriving (Eq, Ord, Read, Show)

evolutionThreadView :: MsMendelConfig -> MVar Job -> IO ThreadView
evolutionThreadView mmc jobs = do
	pop <- V.generateM (mmcInitialPopulation mmc) \_ -> newGenome (mmcPatternWidth mmc) (mmcPatternHeight mmc) (mmcInitialPatterns mmc) (mmcInitialPatternThreshold mmc)
	replies <- newEmptyMVar
	rng <- createSystemRandom
	overviewRef <- newTVarIO GenerationOverview
		{ goID = 0
		, goPopulationSize = V.length pop
		, goPopulationEvaluated = 0
		}

	genDesc <- new Label [#label := "generation"]
	popDesc <- new Label [#label := "population size"]
	evalDesc <- new Label [#label := "evaluated population"]
	genVal <- new Label []
	popVal <- new Label []
	evalVal <- new Label []

	let refresh = do
	    	overview <- readTVarIO overviewRef
	    	set genVal [#label := tshow (goID overview)]
	    	set popVal [#label := tshow (goPopulationSize overview)]
	    	set evalVal [#label := tshow (goPopulationEvaluated overview)]

	table <- new Grid []
	#attach table genDesc 0 0 1 1
	#attach table popDesc 0 1 1 1
	#attach table evalDesc 0 2 1 1
	#attach table genVal 1 0 1 1
	#attach table popVal 1 1 1 1
	#attach table evalVal 1 2 1 1

	tvNew table refresh (evolutionThread mmc jobs replies overviewRef rng pop)

evolutionThread :: MsMendelConfig -> MVar Job -> MVar Evaluation -> TVar GenerationOverview -> GenIO -> Vector Genome -> StatusCheck -> IO ()
evolutionThread mmc jobs replies overviewRef rng pop0 sc = go pop0 where
	go pop = do
		scIO_ sc
		gs0 <- initialState rng
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
		evals <- replicateM (V.length pop) do
			scIO sc do
				forkIO . forever $ takeMVar replies
				killThread tid
			v <- takeMVar replies
			atomically $ v <$ modifyTVar overviewRef \overview -> overview { goPopulationEvaluated = goPopulationEvaluated overview + 1 }
		print (eViruses <$> evals)
		atomically $ modifyTVar overviewRef \overview -> overview { goID = goID overview + 1, goPopulationEvaluated = 0 }
		go pop
