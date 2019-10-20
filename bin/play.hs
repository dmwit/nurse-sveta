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
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Builder (Builder)
import Data.Char
import Data.Fixed
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Maybe
import Data.Time
import Data.Vector.Unboxed (Vector)
import Data.Word
import Dr.Mario.Model as M
import Dr.Mario.Sveta
import Dr.Mario.Sveta.MCTS
import Numeric
import Options.Applicative
import System.Exit
import System.IO
import System.Random.MWC
import Util as U
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as V
import qualified Dr.Mario.Protocol.Raw as Proto
import qualified Graphics.Vty as Vty
import qualified Options.Applicative.Help.Pretty as D
import qualified Options.Applicative.Help.Chunk as D

main :: IO ()
main = do
	opts <- execParser evaluationOptions
	(board, genPill) <- readBoardSelection (boardSelection opts) >>= evalBoardSelection
	(c1, c2) <- genPill

	gen <- createSystemRandom -- we want this to be independent of the randomness, if any, used in genPill
	params <- dmReroot (dmParameters gen board) [ChanceMove (Colors WithMirror Exact c1 c2)]
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

	mh <- traverse (\fp -> openFile fp WriteMode) (logFile opts)
	let hs = [stdout | ui opts == Log] ++ toList mh
	traverse_ (\h -> B.hPutBuilder h (ppLn board)) hs

	case ui opts of
		Visual -> visual  timerChan hs board c1 c2
		_      -> textual timerChan hs

	traverse_ hClose mh

data EvaluationOptions = EvaluationOptions
	{ moveTime :: Micro
	, ui :: UIFormat
	, logFile :: Maybe FilePath
	, boardSelection :: Maybe FilePath
	} deriving (Eq, Ord, Read, Show)

data UIFormat = Visual | Log | None deriving (Bounded, Enum, Eq, Ord, Read, Show)

data LevelSelection = ExactLevel Int | RandomLevel deriving (Eq, Ord, Read, Show)
data SeedSelection = ExactSeed (Vector Word32) | RandomSeed deriving (Eq, Ord, Read, Show)
data BoardSelection
	= ExactBoard Board [(Color, Color)]
	| RandomBoard LevelSelection SeedSelection
	deriving (Eq, Ord, Read, Show)

type Q a = ([a], [a])

push :: a -> Q a -> Q a
push a (old, new) = (old, a:new)

pop :: Q a -> (a, Q a)
pop (a:old, new) = (a, (old, new))
pop ([], new) = pop (reverse new, [])

evaluationOptions :: ParserInfo EvaluationOptions
evaluationOptions = info (helper <*> parser)
	(  fullDesc
	<> progDesc "Use a Monte-Carlo tree search-based AI to play a game of Dr. Mario"
	) where
	parser = pure EvaluationOptions
		<*> option auto
			(  short 't'
			<> help "How long to spend thinking about each move"
			<> value 0.5
			<> showDefault
			<> metavar "SECONDS"
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
	uiFormat = maybeReader $ \case
		"gui" -> Just Visual
		"machine" -> Just Log
		"quiet" -> Just None
		_ -> Nothing
	helpChunk = helpDoc . D.unChunk
	(<<$$>>) = D.chunked (D.<$$>)

readBoardSelection :: Maybe FilePath -> IO BoardSelection
readBoardSelection Nothing = pure (RandomBoard RandomLevel RandomSeed)
readBoardSelection (Just fp) = do
	bs <- LC8.readFile fp
	case A.parse parseBoardSelection bs of
		A.Done bs' selection
			| LC8.null bs' -> pure selection
			| otherwise -> die
				$  "Extra junk at end of file " ++ fp ++ ".\n"
				++ "The first little bit looks like this:\n"
				++ show (LC8.take 40 bs')
		A.Fail bs' ctxts err -> die
			$  "Parsing of " ++ fp ++ " failed:\n"
			++ err ++ "\n"
			++ "Context stack:\n"
			++ unlines ctxts

parseBoardSelection :: A.Parser BoardSelection
parseBoardSelection = parseExactBoard <|> parseRandomBoard where
	parseExactBoard = liftA2 ExactBoard
		(parseAndWarn <* newline)
		(some (liftA2 (,) parseColor parseColor <* newline))
	parseColor = asum
		[ Blue   <$ A.word8 98
		, Red    <$ A.word8 114
		, Yellow <$ A.word8 121
		]
	parseRandomBoard = liftA2 RandomBoard parseLevelSelection parseSeedSelection
	parseLevelSelection = (parseExactLevel <|> parseRandomLevel) <* newline
	parseExactLevel = do
		d1 <- A.satisfy isDigit
		md2 <- optional (A.satisfy isDigit)
		let level = case md2 of
		    	Just d2 -> fromDigits d1 d2
		    	Nothing -> fromDigit d1
		unless (level <= 20) (fail "expected a level in the 0-20 range")
		pure (ExactLevel level)
	parseRandomLevel = RandomLevel <$ random
	parseSeedSelection = (parseExactSeed <|> parseRandomSeed) <* newline
	parseExactSeed = do
		ws <- A.sepBy1 parseAndWarn space
		let len = length ws
		unless (len == 258 || len <= 256) . fail
			$ "expected either 258 or at most 256 words; saw "
			++ show len
			++ " instead"
		pure (ExactSeed (V.fromList ws))
	parseRandomSeed = RandomSeed <$ random
	parseAndWarn :: Proto.Protocol a => A.Parser a
	parseAndWarn = do
		(v, warnings) <- Proto.parse
		unless (null warnings) ((fail . unlines . map show) warnings)
		pure v
	random = A.string (C8.pack "random")
	newline = A.word8 10
	space = A.word8 32
	fromDigit = subtract 48 . fromEnum
	fromDigits a b = 10 * fromDigit a + fromDigit b
	isDigit w = 48 <= w && w <= 57

evalBoardSelection :: BoardSelection -> IO (Board, IO (Color, Color))
evalBoardSelection (ExactBoard b ps) = do
	qRef <- newIORef (ps, [])
	pure . (,) b $ do
		(p, q) <- pop <$> readIORef qRef
		writeIORef qRef (push p q)
		pure p
evalBoardSelection (RandomBoard levelSelection seedSelection) = do
	gen <- case seedSelection of
		ExactSeed seed -> initialize seed
		RandomSeed -> createSystemRandom
	b <- case levelSelection of
		ExactLevel level -> liftA2 M.randomBoard (uniformR (2, maxBound) gen) (pure level)
		RandomLevel -> U.randomBoard gen
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

findChanceMove :: Color -> Color -> HashMap MCMove a -> Maybe a
findChanceMove l r m = asum . map (flip HM.lookup m . ChanceMove) . tail $ [undefined
	, Colors WithMirror Exact lo hi
	, Colors Positional Exact l r
	, Colors WithMirror Approximate lo hi
	-- Colors Positional Approximate l r currently can never happen
	]
	where
	lo = min l r
	hi = max l r

bestMove :: Color -> Color -> DrMarioTree -> DrMarioParameters -> IO (GameUpdate, DrMarioTree)
bestMove c1 c2 t params = case maximumOn (meanUtility . statistics . snd) (HM.toList (children t)) of
	Nothing
		| null (unexplored t) -> error "The impossible happened! The game was not yet won or lost, but there were no valid moves."
		| otherwise -> pure (Timeout, t)
	Just (AIMove p, t')
		| null (children t') && null (unexplored t') -> pure (Ended p, t')
		| otherwise -> case findChanceMove c1 c2 (children t') of
			Just t'' -> pure (Continue p c1 c2, t'')
			Nothing -> do
				mcpos <- root params
				play params mcpos (AIMove p)
				play params mcpos (ChanceMove (Colors Positional Exact c1 c2))
				ms <- expand params mcpos
				pure (Continue p c1 c2, MCTree
					{ statistics = mempty
					, children = HM.empty
					, unexplored = toList ms
					})
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
		next <- bestMove c1 c2 (tree comms) (params comms)
		case next of
			(u@(Continue p l r), tree') -> do
				params' <- dmReroot (params comms) [AIMove p, ChanceMove (Colors Positional Exact l r)]
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

showUpdate :: GameUpdate -> Builder
showUpdate = \case
	Ended p -> ppLn p <> endBuilder
	Timeout -> timeoutBuilder
	Stall -> stallBuilder
	Continue p _ _ -> ppLn p
	where
	[endBuilder, timeoutBuilder, stallBuilder]
		= map B.string8 ["end\n","timeout\n","stall\n"]

ppLn :: Proto.Protocol a => a -> Builder
ppLn a = snd (Proto.pp a) <> B.char8 '\n'

outputUpdate :: [Handle] -> GameUpdate -> IO ()
outputUpdate hs update = traverse_ (\h -> B.hPutBuilder h b) hs where
	b = showUpdate update

textual :: BChan (Double, GameUpdate) -> [Handle] -> IO ()
textual timerChan hs = do
	(_, update) <- readBChan timerChan
	outputUpdate hs update
	case update of
		Continue{} -> textual timerChan hs
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

visual :: BChan (Double, GameUpdate) -> [Handle] -> Board -> Color -> Color -> IO ()
visual timerChan hs b c1 c2 = do
	now <- getCurrentTime
	let p = Progress { pIterations = 0, pTime = now }
	customMain (Vty.mkVty Vty.defaultConfig) (Just timerChan) (app hs) UIState
		{ board = b
		, nextPill = Just (c1, c2)
		, startProgress = p
		, lastProgress = p
		, queuedUpdate = Nothing
		, queuedProgress = Nothing
		}
	pure ()

app :: [Handle] -> App UIState (Double, GameUpdate) ()
app hs = App
	{ appDraw = renderUIState
	, appChooseCursor = neverShowCursor
	, appHandleEvent = handleEvent hs
	, appStartEvent = pure
	, appAttrMap = const (attrMap Vty.defAttr [])
	}

handleEvent :: [Handle] -> UIState -> BrickEvent () (Double, GameUpdate) -> EventM () (Next UIState)
handleEvent hs s = \case
	AppEvent (iterationsNow, update) -> do
		timeNow <- liftIO getCurrentTime
		liftIO (outputUpdate hs update)
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
		++ (LC8.unpack . B.toLazyByteString . showUpdate) update
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
	boardImage = renderBoardWithPill (board s) (queuedUpdate s >>= pillFromGameUpdate)
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
