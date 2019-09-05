{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

import Brick as B
import Brick.BChan
import Brick.Widgets.Center
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Thread.Delay
import Control.Monad.IO.Class
import Data.Char
import Data.Fixed
import Data.IORef
import Data.Maybe
import Data.Time
import Data.Vector.Unboxed (Vector)
import Data.Word
import Dr.Mario.Model
import Dr.Mario.Sveta
import Dr.Mario.Sveta.MCTS
import Numeric
import Options.Applicative
import System.IO
import System.Random.MWC
import Util as U
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as V
import qualified Graphics.Vty as Vty
import qualified Options.Applicative.Help.Pretty as D
import qualified Options.Applicative.Help.Chunk as D

main :: IO ()
main = do
	opts <- execParser evaluationOptions
	(board, genPill) <- readStartingConditions (startingConditions opts) >>= evalStartingConditions
	(c1, c2) <- genPill

	gen <- createSystemRandom -- we want this to be independent of the randomness, if any, used in genPill
	params <- dmReroot (dmParameters gen board) [ChanceMove c1 c2]
	tree <- emptyTree params

	commsRef <- newTVarIO Comms
		{ tree = tree
		, params = params
		, repetitions = Infinity
		, sequenceNumber = minBound
		, iterations = 0
		}
	timerChan <- newBChan 1
	forkIO (mctsThread commsRef)
	forkIO (timerThread (moveTime opts) timerChan commsRef genPill)

	(outputUpdate, closeOutput) <- openOutput (logFile opts)

	case ui opts of
		Visual -> visual  timerChan outputUpdate board c1 c2
		Log    -> textual timerChan (\u -> outputUpdate u >> putStr (showUpdate u))
		None   -> textual timerChan outputUpdate

	closeOutput

data EvaluationOptions = EvaluationOptions
	{ moveTime :: Micro
	, ui :: UIFormat
	, logFile :: Maybe FilePath
	, startingConditions :: Maybe FilePath
	} deriving (Eq, Ord, Read, Show)

data UIFormat = Visual | Log | None deriving (Bounded, Enum, Eq, Ord, Read, Show)

data StartingConditions
	= MWC (Vector Word32)
	| Exact Board [(Color, Color)]
	| SystemRandom
	deriving (Eq, Ord, Read, Show)

type Q a = ([a], [a])

push :: a -> Q a -> Q a
push a (old, new) = (old, a:new)

pop :: Q a -> (a, Q a)
pop (a:old, new) = (a, (old, new))
pop ([], new) = pop (reverse new, [])

evaluationOptions :: ParserInfo (EvaluationOptions)
evaluationOptions = info (helper <*> parser)
	(  fullDesc
	<> progDesc "Use a Monte-Carlo tree search-based AI to play a game of Dr. Mario"
	)
	where
	parser = pure EvaluationOptions
		<*> option auto
			(  short 't'
			<> help "How long (in seconds) to spend thinking about each move"
			<> value 0.5
			<> showDefault
			<> metavar "NUMBER"
			)
		<*> option uiFormat
			(  long "ui"
			<> helpChunk
				(      D.paragraph "gui: Show moves in a GUI as they are selected (default)"
				<<$$>> D.paragraph "machine: Print moves to stdout in a machine-readable format as they are selected"
				<<$$>> D.paragraph "quiet: No display"
				)
			<> value Visual
			<> metavar "UI"
			)
		<*> optional (strOption
			(  long "output"
			<> short 'o'
			<> help "Print moves in machine-readable format to a file"
			<> metavar "FILE"
			))
		<*> optional (strOption
			(  long "input"
			<> short 'i'
			<> help "File to read to set up initial board and pill sequence (if none specified, choose randomly)"
			<> metavar "FILE"
			))
	uiFormat = maybeReader \case
		"gui" -> Just Visual
		"machine" -> Just Log
		"quiet" -> Just None
		_ -> Nothing
	helpChunk = helpDoc . D.unChunk
	(<<$$>>) = D.chunked (D.<$$>)

readStartingConditions :: Maybe FilePath -> IO StartingConditions
readStartingConditions Nothing = pure SystemRandom
readStartingConditions (Just fp) = pure SystemRandom -- TODO

evalStartingConditions :: StartingConditions -> IO (Board, IO (Color, Color))
evalStartingConditions (Exact b ps) = do
	qRef <- newIORef (ps, [])
	pure . (,) b $ do
		(p, q) <- pop <$> readIORef qRef
		writeIORef qRef (push p q)
		pure p
evalStartingConditions randomSource = do
	gen <- case randomSource of
		SystemRandom -> createSystemRandom
		MWC seed -> initialize seed
	b <- U.randomBoard gen
	pure (b, U.randomPill gen)

data GameUpdate
	= Ended Pill
	| Timeout -- no rollouts had been completed within the time allotted for making a move
	| Stall -- too many pills went by without clearing any viruses
	| Continue Pill Color Color
	deriving (Eq, Ord, Read, Show)

maximumOn :: Ord b => (a -> b) -> [a] -> Maybe a
maximumOn f [] = Nothing
maximumOn f (a:as) = go a (f a) as where
	go a b [] = Just a
	go a b (a':as) = let b' = f a' in if b' > b then go a' b' as else go a b as

bestMove :: Color -> Color -> DrMarioTree -> (GameUpdate, DrMarioTree)
bestMove c1 c2 t = case maximumOn (meanUtility . statistics . snd) (HM.toList (children t)) of
	Nothing
		| null (unexplored t) -> error "The impossible happened! The game was not yet won or lost, but there were no valid moves."
		| otherwise -> (Timeout, t)
	Just (AIMove p, t')
		| null (children t') && null (unexplored t') -> (Ended p, t')
		| otherwise -> case HM.lookup (ChanceMove c1 c2) (children t') of
			-- We could expand the node here instead of timing out, but this
			-- probably will never happen anyway, so...?
			Nothing -> (Timeout, t)
			Just t'' -> (Continue p c1 c2, t'')
	Just (m, _) -> error
		$  "The impossible happened! It was the AI's turn, but the best available move was"
		++ show m

timerThread :: Micro -> BChan (Double, GameUpdate) -> TVar Comms -> IO (Color, Color) -> IO ()
timerThread (MkFixed micros) timerChan commsRef genPill = go stallThreshold where
	go 0 = finish Stall
	go n = do
		delay micros
		(c1, c2) <- genPill
		comms <- atomically (readTVar commsRef)
		case bestMove c1 c2 (tree comms) of
			(u@(Continue p l r), tree') -> do
				params' <- dmReroot (params comms) [AIMove p, ChanceMove l r]
				atomically $ writeTVar commsRef comms
					{ tree = tree'
					, params = params'
					, sequenceNumber = sequenceNumber comms + 1
					}
				writeBChan timerChan (iterations comms, u)
				mcpos <- root (params comms)
				mVirusesCleared <- mplace (mboard mcpos) p
				case mVirusesCleared of
					Nothing -> error
						$  "The impossible happened in timerThread!\n"
						++ "The AI chose the invalid move " ++ show p
					Just 0 -> go (n-1)
					_ -> go stallThreshold
			(ending, _) -> finish ending

	finish ending = do
		comms <- atomically (readTVar commsRef)
		atomically $ writeTVar commsRef comms
			{ repetitions = Finite 0
			, sequenceNumber = sequenceNumber comms + 1
			}
		writeBChan timerChan (iterations comms, ending)

showUpdate :: GameUpdate -> String
showUpdate (Ended p) = showPill p ++ "end\n"
showUpdate Timeout = "timeout\n"
showUpdate Stall = "stall\n"
showUpdate (Continue p _ _) = showPill p

showPill :: Pill -> String
showPill p = id
	. shows (x (bottomLeftPosition p))
	. (' ':)
	. shows (y (bottomLeftPosition p))
	. (' ':)
	. (firstChar (bottomLeftColor (content p)):)
	. (firstChar (     otherColor (content p)):)
	. (' ':)
	. (firstChar (orientation     (content p)):)
	$ "\n"
	where
	firstChar :: Show a => a -> Char
	firstChar = toLower . head . show

openOutput :: Maybe FilePath -> IO (GameUpdate -> IO (), IO ())
openOutput Nothing = pure (\_ -> pure (), pure ())
openOutput (Just fp) = do
	h <- openFile fp WriteMode
	pure (hPutStr h . showUpdate, hClose h)

textual :: BChan (Double, GameUpdate) -> (GameUpdate -> IO ()) -> IO ()
textual timerChan outputUpdate = do
	(_, update) <- readBChan timerChan
	outputUpdate update
	case update of
		Continue{} -> textual timerChan outputUpdate
		_ -> pure ()

data Progress = Progress
	{ pIterations :: Double
	, pTime :: UTCTime
	} deriving (Eq, Ord, Read, Show)

data UIState = UIState
	{ board :: Board
	, nextPill :: Maybe (Color, Color)
	, startProgress :: Progress
	, lastProgress :: Progress
	, queuedUpdate :: Maybe GameUpdate
	, queuedProgress :: Maybe Progress
	} deriving (Eq, Ord, Read, Show)

visual :: BChan (Double, GameUpdate) -> (GameUpdate -> IO ()) -> Board -> Color -> Color -> IO ()
visual timerChan outputUpdate b c1 c2 = do
	now <- getCurrentTime
	let p = Progress { pIterations = 0, pTime = now }
	customMain (Vty.mkVty Vty.defaultConfig) (Just timerChan) (app outputUpdate) UIState
		{ board = b
		, nextPill = Just (c1, c2)
		, startProgress = p
		, lastProgress = p
		, queuedUpdate = Nothing
		, queuedProgress = Nothing
		}
	pure ()

app :: (GameUpdate -> IO ()) -> App UIState (Double, GameUpdate) ()
app outputUpdate = App
	{ appDraw = renderUIState
	, appChooseCursor = neverShowCursor
	, appHandleEvent = handleEvent outputUpdate
	, appStartEvent = pure
	, appAttrMap = const (attrMap Vty.defAttr [])
	}

handleEvent :: (GameUpdate -> IO ()) -> UIState -> BrickEvent () (Double, GameUpdate) -> EventM () (Next UIState)
handleEvent outputUpdate s = \case
	AppEvent (iterationsNow, update) -> do
		timeNow <- liftIO getCurrentTime
		liftIO (outputUpdate update)
		continue s
			{ board = maybe id placeGameUpdate (queuedUpdate s) (board s)
			, nextPill = lookaheadFromGameUpdate update
			, lastProgress = fromMaybe (lastProgress s) (queuedProgress s)
			, queuedUpdate = Just update
			, queuedProgress = Just Progress
				{ pIterations = iterationsNow
				, pTime = timeNow
				}
			}
	VtyEvent e -> case e of
		Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> halt s
		_ -> continue s
	_ -> continue s

pillFromGameUpdate :: GameUpdate -> Maybe Pill
pillFromGameUpdate (Ended p) = Just p
pillFromGameUpdate (Continue p _ _) = Just p
pillFromGameUpdate _ = Nothing

lookaheadFromGameUpdate :: GameUpdate -> Maybe (Color, Color)
lookaheadFromGameUpdate (Continue _ l r) = Just (l, r)
lookaheadFromGameUpdate _ = Nothing

placeGameUpdate :: GameUpdate -> Board -> Board
placeGameUpdate update b = case pillFromGameUpdate update of
	Nothing -> error
		$ "The impossible happened! Received more game updates after the game ostensible ended by a "
		++ showUpdate update
		++ "."
	Just p -> case place b p of
		Nothing -> error
			$  "The impossible happened in placeGameUpdate!\n"
			++ "The AI chose the invalid move " ++ show p
		Just (_, b') -> b'

renderUIState :: UIState -> [Widget ()]
renderUIState s = pure . joinBorders . vBox $
	[ hCenter (raw (lookaheadImage Vty.<-> boardImage))
	, B.str " "
	, status
	, B.str " "
	, maybe (B.str " ") (renderProgress "Move" (lastProgress s)) (queuedProgress s)
	, maybe (B.str " ") (renderProgress "Game" (startProgress s)) (queuedProgress s)
	] where
	lookaheadImage = case nextPill s of
		Nothing -> Vty.char Vty.defAttr ' '
		Just (l,r) -> renderLookaheadFor (board s) l r
	boardImage = renderBoard (board s) $ case queuedUpdate s >>= pillFromGameUpdate of
		Nothing -> const Nothing
		Just p -> pillOverlay p
	status = hCenter . B.str $ case queuedUpdate s of
		Nothing -> "Running..."
		Just Continue{} -> "Running..."
		Just Ended{} -> "The game has concluded normally."
		Just Timeout -> "Failed to complete one rollout in the timeout allotted."
		Just Stall -> "Stalemate: " ++ show (floor stallThreshold) ++ " pills have been placed without clearing any viruses."

renderProgress :: String -> Progress -> Progress -> Widget n
renderProgress durationDescription old new = hCenter . B.str
	$  durationDescription
	++ ": "
	++ pad 8 (show (floor dIterations))
	++ " rollouts /"
	++ pad 8 (showFFloat (Just 2) dTime "")
	++ "s = "
	++ pad 6 (show (round (dIterations / dTime)))
	++ "rps"
	where
	dIterations = pIterations new - pIterations old
	dTime = realToFrac (diffUTCTime (pTime new) (pTime old)) :: Double
	pad n s = replicate (n - length s) ' ' ++ s
