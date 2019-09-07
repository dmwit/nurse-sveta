{-# LANGUAGE LambdaCase #-}
import Brick
import Brick.BChan
import Brick.Widgets.Center
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Fixed
import Data.List
import Data.Map.Strict (Map)
import GHC.Conc
import Graphics.Vty
import Lens.Micro
import Options.Applicative hiding (str)
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import qualified Data.Map.Strict as M
import qualified Options.Applicative as OA

main :: IO ()
main = do
	caps <- getNumProcessors
	opts <- execParser (options caps)

	forM_ (timeouts opts) $ \t ->
		createDirectoryIfMissing True (outputDirectory opts </> show t)

	replyChan <- newBChan 1
	instructionChan <- newChan
	forM_ ((reverse . sort . timeouts) opts) $ \t ->
		forM_ (inputFiles opts) $ \fp ->
			writeChan instructionChan (Play t fp)
	replicateM_ (numJobs opts) $ do
		writeChan instructionChan Die
		forkIO (jobRunner instructionChan replyChan (outputDirectory opts))

	customMain (mkVty defaultConfig) (Just replyChan) app UIState
		{ topFile = head (inputFiles opts) -- safe because the parser uses some
		, numRunners = numJobs opts
		, statuses = allPending (inputFiles opts) (timeouts opts)
		}

	pure ()

data Options = Options
	{ timeouts :: [Micro]
	, inputFiles :: [FilePath]
	, outputDirectory :: FilePath
	, numJobs :: Int
	} deriving (Eq, Ord, Read, Show)

options :: Int -> ParserInfo Options
options caps = info (helper <*> parser)
	(  fullDesc
	<> progDesc "Play many games of Dr. Mario simultaneously, with a job pool"
	) where
	parser = pure Options
		<*> some (option auto
			(  short 't'
			<> long "timeout"
			<> help "A time limit to spend thinking about each move (can be specified many times)"
			<> metavar "NUMBER"
			))
		<*> some (argument OA.str
			(  help "A description of a board and pills to present to the AI (can be specified many times)"
			<> metavar "FILE"
			))
		<*> strOption
			(  short 'o'
			<> long "output"
			<> help "A directory to put results in"
			<> metavar "DIR"
			)
		<*> option auto
			(  short 'j'
			<> help "How many jobs to run simultaneously"
			<> metavar "INTEGER"
			<> value (caps-1)
			<> showDefault
			)

data Instruction = Play Micro FilePath | Die
	deriving (Eq, Ord, Read, Show)

data Status = Pending | Executing | Succeeded | Failed deriving (Bounded, Enum, Eq, Ord, Read, Show)
data Reply = StatusUpdate Micro FilePath Status | Dead deriving (Eq, Ord, Read, Show)

jobRunner :: Chan Instruction -> BChan Reply -> FilePath -> IO ()
jobRunner instructionChan replyChan outDir = go where
	go = do
		i <- readChan instructionChan
		case i of
			Die -> writeBChan replyChan Dead
			Play t fp -> do
				writeBChan replyChan (StatusUpdate t fp Executing)
				(code, out, err) <- readProcessWithExitCode "nurse-sveta-play"
					[ "-t", show t
					, "-i", fp
					, "-o", replaceDirectory fp (outDir </> show t) -<.> "out"
					, "--ui", "quiet"
					]
					""
				writeBChan replyChan . StatusUpdate t fp $ case code of
					ExitSuccess -> Succeeded
					_ -> Failed
				go

data UIState = UIState
	{ statuses :: Map FilePath (Map Micro Status)
	, topFile :: FilePath
	, numRunners :: Int
	} deriving (Eq, Ord, Read, Show)

allPending :: [FilePath] -> [Micro] -> Map FilePath (Map Micro Status)
allPending fps ts = M.fromList [(fp, tMap) | fp <- fps] where
	tMap = M.fromList [(t, Pending) | t <- ts]

app :: App UIState Reply ()
app = App
	{ appDraw = renderUIState
	, appChooseCursor = neverShowCursor
	, appHandleEvent = handleEvent
	, appStartEvent = pure
	, appAttrMap = const (attrMap defAttr [])
	}

handleEvent :: UIState -> BrickEvent () Reply -> EventM () (Next UIState)
handleEvent s = \case
	AppEvent Dead -> continue s { numRunners = numRunners s - 1 }
	-- TODO: try to scroll so that this is in view
	AppEvent (StatusUpdate t fp status) -> continue s
		{ statuses = M.insertWith M.union fp (M.singleton t status) (statuses s) }
	VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt s
	VtyEvent (EvKey k []) -> case k of
		KUp   -> continue s { topFile = reTop (M.lookupMax . fst) }
		KDown -> continue s { topFile = reTop (M.lookupMin . snd) }
		_ -> continue s
	_ -> continue s
	where
	reTop f = id
		. maybe (topFile s) fst
		. f
		. M.split (topFile s)
		$ statuses s

renderUIState :: UIState -> [Widget ()]
renderUIState s = pure . joinBorders . Widget Greedy Greedy $ do
	h <- asks (^.availWidthL)
	id
		. render
		. vBox
		. (renderRunners (numRunners s):)
		. (renderHeader s:)
		. map renderFilePathStatuses
		. M.toAscList
		. M.take (h-2)
		. splitGE (topFile s)
		. statuses
		$ s

splitGE :: Ord k => k -> Map k v -> Map k v
splitGE k m = maybe id (M.insert k) mv gt where
	(_, mv, gt) = M.splitLookup k m

renderRunners :: Int -> Widget n
renderRunners n = hCenter . str $ show n ++ " processes executing"

renderHeader :: UIState -> Widget n
-- elemAt 0 is safe because the options parser uses some for both input files and timeouts
renderHeader = padLeft Max . str . unwords . map (show . fst) . M.toAscList . snd . M.elemAt 0 . statuses

renderFilePathStatuses :: (FilePath, Map Micro Status) -> Widget n
renderFilePathStatuses (fp, m) = hBox
	$  [ padLeft Max (str fp)
	   , str "      "
	   ]
	++ intersperse (str " ") (map renderStatus (M.toAscList m))

renderStatus :: (Micro, Status) -> Widget n
renderStatus (t, status) = str . padTo (length (show t)) $ case status of
	Pending -> ' '
	Executing -> '▶'
	Succeeded -> '✓'
	Failed -> 'x'
	where
	padTo n c = replicate (n`div`2) ' ' ++ [c] ++ replicate (n - n`div`2 - 1) ' '
