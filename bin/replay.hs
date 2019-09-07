{-# LANGUAGE LambdaCase #-}

import Brick
import Brick.BChan
import Brick.Widgets.Center
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Thread.Delay
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import Data.Char
import Data.Fixed
import Data.Foldable
import Graphics.Vty (Event(..), Key(..), Modifier(..), char, defAttr, defaultConfig, mkVty)
import Dr.Mario.Model
import Dr.Mario.Protocol.Raw
import Options.Applicative hiding (str)
import System.Exit
import Util (renderBoardWithPill, renderLookaheadFor)
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Graphics.Vty as Vty
import qualified Options.Applicative as OA

main :: IO ()
main = do
	opts <- execParser options
	record <- readGameRecord (gameRecord opts)
	uiState <- uiStateFromGameRecord record

	timerChan <- newBChan 1
	commsRef <- newTVarIO Comms
		{ delayTime = moveTime opts
		, timerEnabled = False
		}
	forkIO (timerThread commsRef timerChan)

	customMain (Vty.mkVty Vty.defaultConfig) (Just timerChan) (app commsRef) uiState
	pure ()

data Options = Options
	{ moveTime :: Micro
	, gameRecord :: FilePath
	} deriving (Eq, Ord, Read, Show)

options :: ParserInfo Options
options = info (helper <*> parser)
	(  fullDesc
	<> progDesc "Take a record of a game played by Nurse Sveta and display it"
	) where
	parser = pure Options
		<*> option auto
			(  short 't'
			<> help "How long to spend displaying each move"
			<> value 0.75
			<> showDefault
			<> metavar "SECONDS"
			)
		<*> strOption
			(  short 'i'
			<> long "input"
			<> help "The game record to display"
			<> metavar "FILE"
			)

readGameRecord :: FilePath -> IO (Board, [Pill], Ending)
readGameRecord fp = do
	bs <- LBS.readFile fp
	case A.parse gameFormat bs of
		A.Fail bs ctxts err -> die
			$  "Parsing of " ++ fp ++ " failed:\n"
			++ err ++ "\n"
			++ "Context:\n"
			++ unlines ctxts
			++ "First little bit of what was left:\n"
			++ show (LBS.take 60 bs)
		A.Done bs r
			| LBS.null bs -> pure r
			| otherwise -> die
				$  "Leftover junk at end of " ++ fp ++ ", starting like this:\n"
				++ show (LBS.take 60 bs)

data Ending = End | Timeout | Stall deriving (Bounded, Enum, Eq, Ord, Read, Show)

describeEnding :: Ending -> String
describeEnding End = "The game ended normally."
describeEnding Timeout = "The AI timed out without finishing a single rollout from this position."
describeEnding Stall = "Stalemate: gameplay halted for placing too many pills without clearing a virus."

gameFormat :: A.Parser (Board, [Pill], Ending)
gameFormat = liftA3 (,,)
	(parseAndWarn <* newline)
	(some (parseAndWarn <* newline))
	(ending <* newline)
	where
	newline = A.word8 10
	ending = asum [e <$ (A.string . C8.pack . map toLower . show) e | e <- [minBound .. maxBound]]

parseAndWarn :: Protocol a => A.Parser a
parseAndWarn = do
	(a, ws) <- parse
	case ws of
		[] -> pure ()
		_ -> fail (show ws)
	pure a

data BoardState = BoardState
	{ board :: Board
	, placement :: Maybe Pill
	, lookahead :: Maybe (Color, Color)
	} deriving (Eq, Ord, Read, Show)

type Zipper a = ([a], a, [a])

advance :: Zipper a -> Zipper a
advance z@(bs, h, []) = z
advance (bs, h, e:es) = (h:bs, e, es)

retreat :: Zipper a -> Zipper a
retreat z@([], h, es) = z
retreat (b:bs, h, es) = (bs, b, h:es)

advanced :: Zipper a -> Bool
advanced (_, _, []) = True
advanced _ = False

extract :: Zipper a -> a
extract (_, h, _) = h

data Comms = Comms
	{ delayTime :: Micro
	, timerEnabled :: Bool
	} deriving (Eq, Ord, Read, Show)

timerThread :: TVar Comms -> BChan () -> IO ()
timerThread commsRef timerChan = forever $ do
	MkFixed t <- atomically $ do
		comms <- readTVar commsRef
		guard (timerEnabled comms)
		pure (delayTime comms)
	writeBChan timerChan ()
	delay t

data UIState = UIState
	{ boards :: Zipper BoardState
	, ending :: Ending
	} deriving (Eq, Ord, Read, Show)

boardStatesFromPills :: Board -> [Pill] -> Either (Board, Pill) [BoardState]
boardStatesFromPills b [] = Right [BoardState b Nothing Nothing]
boardStatesFromPills b (p:ps) = (BoardState b Nothing (Just (lookaheadFromPill p)):) <$> go b p ps where
	go b p [] = Right [BoardState b (Just p) Nothing]
	go b p (p':ps) = case place b p of
		Nothing -> Left (b, p)
		Just (_, b') -> (BoardState b (Just p) (Just (lookaheadFromPill p')):)
			<$> go b' p' ps

	lookaheadFromPill :: Pill -> (Color, Color)
	lookaheadFromPill p = (bottomLeftColor (content p), otherColor (content p))

uiStateFromBoardStates :: Ending -> [BoardState] -> UIState
uiStateFromBoardStates e (b:bs) = UIState
	{ boards = ([], b, bs)
	, ending = e
	}

uiStateFromGameRecord :: (Board, [Pill], Ending) -> IO UIState
uiStateFromGameRecord (b, ps, e) = case boardStatesFromPills b ps of
	Left (b', p) -> die
		$  "Found illegal pill placement in game record.\n"
		++ "Pill: " ++ show p ++ "\n"
		++ "Board: " ++ show b'
	Right [] -> error "The impossible happened! boardStatesFromPills returned an empty list."
	Right (bs:bss) -> pure UIState
		{ boards = ([], bs, bss)
		, ending = e
		}

app :: TVar Comms -> App UIState () ()
app commsRef = App
	{ appDraw = renderUIState
	, appChooseCursor = neverShowCursor
	, appHandleEvent = handleEvent commsRef
	, appStartEvent = pure
	, appAttrMap = const (attrMap Vty.defAttr [])
	}

handleEvent :: TVar Comms -> UIState -> BrickEvent n () -> EventM n (Next UIState)
handleEvent commsRef s = \case
	AppEvent () -> onBoards advance s
	VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt s
	VtyEvent (EvKey k []) -> case k of
		KLeft -> disableTimer commsRef >> onBoards retreat s
		KUp -> disableTimer commsRef >> onBoards retreat s
		KRight -> disableTimer commsRef >> onBoards advance s
		KDown -> disableTimer commsRef >> onBoards advance s
		KChar ' ' -> toggleTimer commsRef >> continue s
		KChar '+' -> do
			onComms commsRef $ \c -> c
				{ delayTime = delayTime c * 0.8
				, timerEnabled = True
				}
			continue s
		KChar '-' -> do
			onComms commsRef $ \c -> c
				{ delayTime = delayTime c * 1.25
				, timerEnabled = True
				}
			continue s
		_ -> continue s
	_ -> continue s

onBoards :: (Zipper BoardState -> Zipper BoardState) -> UIState -> EventM n (Next UIState)
onBoards f s = continue s { boards = f (boards s) }

onComms :: TVar Comms -> (Comms -> Comms) -> EventM n ()
onComms commsRef = liftIO . atomically . modifyTVar commsRef

enableTimer :: TVar Comms -> EventM n ()
enableTimer commsRef = onComms commsRef $ \c -> c { timerEnabled = True }

disableTimer :: TVar Comms -> EventM n ()
disableTimer commsRef = onComms commsRef $ \c -> c { timerEnabled = False }

toggleTimer :: TVar Comms -> EventM n ()
toggleTimer commsRef = onComms commsRef $ \c -> c { timerEnabled = not (timerEnabled c) }

renderUIState :: UIState -> [Widget n]
renderUIState (UIState { boards = bss, ending = e }) = pure . joinBorders . vCenter . vBox $
	[ hCenter (raw (lookaheadImage Vty.<-> boardImage))
	, str " "
	, if advanced bss
	  then hCenter . str . describeEnding $ e
	  else str " "
	]
	where
	bs = extract bss
	lookaheadImage = case lookahead bs of
		Nothing -> char defAttr ' '
		Just (l,r) -> renderLookaheadFor (board bs) l r
	boardImage = renderBoardWithPill (board bs) (placement bs)
