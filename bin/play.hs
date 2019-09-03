{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

import Brick.BChan
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Thread.Delay
import Data.Char
import Data.Fixed
import Data.IORef
import Data.Vector.Unboxed (Vector)
import Data.Word
import Dr.Mario.Model
import Dr.Mario.Sveta
import Dr.Mario.Sveta.MCTS
import Options.Applicative
import Options.Applicative.Help.Pretty as D
import Options.Applicative.Help.Chunk as D
import System.IO
import System.Random.MWC
import Util as U
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as V

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
		Visual -> visual  timerChan commsRef outputUpdate
		Log    -> textual timerChan          (\u -> outputUpdate u >> putStr (showUpdate u))
		None   -> textual timerChan          outputUpdate

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
	= Ended
	| Timeout -- no rollouts had been completed within the time allotted for making a move
	| Stall -- too many pills went by without clearing any viruses
	| Continue [MCMove]
	deriving (Eq, Ord, Read, Show)

maximumOn :: Ord b => (a -> b) -> [a] -> Maybe a
maximumOn f [] = Nothing
maximumOn f (a:as) = go a (f a) as where
	go a b [] = Just a
	go a b (a':as) = let b' = f a' in if b' > b then go a' b' as else go a b as

bestMove :: Color -> Color -> DrMarioTree -> (GameUpdate, DrMarioTree)
bestMove c1 c2 t = case maximumOn (meanUtility . statistics . snd) (HM.toList (children t)) of
	Nothing -> (if null (unexplored t) then Ended else Timeout, t)
	Just (m, t') -> case HM.lookup m' (children t') of
		-- We could expand the node here instead of timing out, but this
		-- probably will never happen anyway, so...?
		Nothing -> (Timeout, t)
		Just t'' -> (Continue [m, m'], t'')
	where m' = ChanceMove c1 c2

timerThread :: Micro -> BChan GameUpdate -> TVar Comms -> IO (Color, Color) -> IO ()
timerThread (MkFixed micros) timerChan commsRef genPill = go stallThreshold where
	go 0 = finish Stall
	go n = do
		delay micros
		(c1, c2) <- genPill
		comms <- atomically (readTVar commsRef)
		case bestMove c1 c2 (tree comms) of
			(Continue ms, tree') -> do
				params' <- dmReroot (params comms) ms
				atomically $ writeTVar commsRef comms
					{ tree = tree'
					, params = params'
					, sequenceNumber = sequenceNumber comms + 1
					}
				writeBChan timerChan (Continue ms)
				case ms of
					AIMove p:_ -> do
						mcpos <- root (params comms)
						mVirusesCleared <- mplace (mboard mcpos) p
						case mVirusesCleared of
							Nothing -> error
								$  "The impossible happened in timerThread!\n"
								++ "The AI chose the invalid move " ++ show p
							Just 0 -> go (n-1)
							_ -> go stallThreshold
					_ -> error
						$  "The impossible happened in timerThread!\n"
						++ "bestMove returned a non-AI move as its first result:\n"
						++ show ms
			(ending, _) -> finish ending

	finish ending = do
		comms <- atomically (readTVar commsRef)
		atomically $ writeTVar commsRef comms
			{ repetitions = Finite 0
			, sequenceNumber = sequenceNumber comms + 1
			}
		writeBChan timerChan ending

showUpdate :: GameUpdate -> String
showUpdate Ended = "end\n"
showUpdate Timeout = "timeout\n"
showUpdate Stall = "stall\n"
showUpdate (Continue ms) = foldr (\m s -> showMove m . s) id ms "" where
	showMove (AIMove p) = id
		. shows (x (bottomLeftPosition p))
		. (' ':)
		. shows (y (bottomLeftPosition p))
		. (' ':)
		. (firstChar (bottomLeftColor (content p)):)
		. (firstChar (     otherColor (content p)):)
		. (' ':)
		. (firstChar (orientation     (content p)):)
		. ('\n':)
	showMove (ChanceMove c1 c2) = id

	firstChar :: Show a => a -> Char
	firstChar = toLower . head . show

openOutput :: Maybe FilePath -> IO (GameUpdate -> IO (), IO ())
openOutput Nothing = pure (\_ -> pure (), pure ())
openOutput (Just fp) = do
	h <- openFile fp WriteMode
	pure (hPutStr h . showUpdate, hClose h)

textual :: BChan GameUpdate -> (GameUpdate -> IO ()) -> IO ()
textual timerChan outputUpdate = do
	update <- readBChan timerChan
	outputUpdate update
	case update of
		Continue ms -> textual timerChan outputUpdate
		_ -> pure ()

visual :: BChan GameUpdate -> TVar Comms -> (GameUpdate -> IO ()) -> IO ()
visual _ _ _ = undefined
