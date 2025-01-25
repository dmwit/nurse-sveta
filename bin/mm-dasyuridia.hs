import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.Foldable
import Data.Function
import Data.List
import Data.Map (Map)
import Data.Vector (Vector)
import Dr.Mario.Model
import Dr.Mario.Pathfinding
import Nurse.Sveta.Files
import Nurse.Sveta.Genome
import System.Environment
import System.Process
import System.IO
import Text.Printf
import Text.Read

import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
	dir <- getXdgDirectory XdgData "ms-mendel"
	generation <- readFile (dir </> "latest.json") >>= readIO @Int
	Just (RecordOfVectors specs) <- A.decodeFileStrict (dir </> show generation <.> "json")
	genome <- gFromSpec (V.head specs)

	args <- getArgs
	(i, o, e, _p) <- runInteractiveProcess "dasyuridia" args Nothing Nothing
	for_ [i, o, e] \h -> hSetBuffering h LineBuffering
	forkIO . forever $ hGetLine e >>= hPrintf stderr "dasyuridia err: %s\n"
	let possiblyEmit' = possiblyEmit i genome
	fforever state0 \s -> hGetLine o >>= \ln -> case parseEvent ln of
		Just e -> case e of
			EBoard b -> possiblyEmit' s { sBoard = Just b }
			ELookahead lk -> possiblyEmit' s { sLookahead = Just lk }
			ESpeed spd -> possiblyEmit' s { sSpeed = Just spd }
			ERelax{} -> pure state0
			ENextControl fc -> possiblyEmit' s { sControl = Just fc }
			_ -> s <$ putStrLn ("I " ++ show e)
		Nothing -> s <$ putStrLn ("E " ++ show ln)

fforever :: Monad m => a -> (a -> m a) -> m b
fforever = flip (fix . (>=>))

data State = State
	{ sBoard :: Maybe Board
	, sLookahead :: Maybe Lookahead
	, sControl :: Maybe FrameCount
	, sSpeed :: Maybe CoarseSpeed
	, sPills :: Int
	} deriving (Eq, Ord, Read, Show)

state0 :: State
state0 = State Nothing Nothing Nothing Nothing 0

possiblyEmit :: Handle -> Genome -> State -> IO State
possiblyEmit h g (State (Just b) (Just lk) (Just fc) (Just spd) pu) = do
	mb <- thaw b
	placements <- mapproxReachable mb (even fc) (gravity spd pu)
	let pbs = V.fromList [((path, pill), b') | (placement, path) <- HM.toList placements, let pill = mpPill placement lk, Just (_, b') <- [place b pill]]
	    scores = gEvaluate g b (snd <$> pbs)
	    bestIndices = V.findIndices (V.maximum scores==) scores
	    ((bestPath, bestPill), _bestB) = pbs V.! V.head bestIndices
	    request = ppPath fc bestPath
	hPutStrLn h request
	printf "R %s: %s\n" (show bestPill) request
	pure state0 { sPills = pu+1 }
possiblyEmit _ _ s = pure s

ppPath :: FrameCount -> MidPath -> String
ppPath fc = printf "%d %s" fc . concatMap ppStep . mpSteps

ppStep :: MidStep -> String
ppStep = \case
	Blink -> "(<ab)"
	Down -> "v"
	MidStep mdir mrot -> case foldMap ppDirection mdir <> foldMap ppRotation mrot of
		[] -> "e"
		s@[_] -> s
		s -> "(" ++ s ++ ")"

ppDirection :: HDirection -> String
ppDirection = \case L -> "<"; R -> ">"

ppRotation :: Rotation -> String
ppRotation = \case Clockwise -> "a"; Counterclockwise -> "b"

type FrameCount = Int
data Event
	= EBoard Board
	| ELookahead Lookahead
	| ESpeed CoarseSpeed
	| ERelax FrameCount
	| ENextControl FrameCount
	| ELock FrameCount Pill
	| EControl FrameCount
	| EAccepted
	| ERejected FrameCount
	| EMalformed
	| EUnprepared
	deriving (Eq, Ord, Read, Show)

parseEvent :: String -> Maybe Event
parseEvent = \case
	c:cs -> M.lookup c tbl >>= ($ cs)
	[] -> Nothing
	where
	-- use Map search for the first character, but linear search for the rest;
	-- the first character almost always uniquely identifies the event anyway
	tbl = M.fromListWith (liftA2 (<|>)) $ tail [undefined
		, "accepted" ~> finished EAccepted
		, "board " ~> \s -> do
			cells <- traverse parseCell (V.fromList s)
			guard (V.length cells == 128)
			pure . EBoard $ unsafeGenerateBoard 8 16 \(Position x y) -> cells V.! (8*y + x)
		, "control " ~> readField EControl
		, "lock " ~> \s -> case words s of
			[frame, orientation, [l, r], '(':x, y] -> do
				pf <- readMaybe frame
				po <- parseOrientation orientation
				pl <- parseColor l
				pr <- parseColor r
				px <- readMaybe =<< dropr ',' x
				py <- readMaybe =<< dropr ')' y
				pure (ELock pf (Pill (PillContent po pl pr) (Position px py)))
			_ -> Nothing
		, "lookahead " ~> \case
			[l, r] -> ELookahead <$> liftA2 Lookahead (parseColor l) (parseColor r)
			_ -> Nothing
		, "malformed" ~> finished EMalformed
		, "next control " ~> readField ENextControl
		, "rejected " ~> readField ERejected
		, "relax " ~> readField ERelax
		, "speed " ~> \case
			"low" -> Just $ ESpeed Low
			"med" -> Just $ ESpeed Med
			"hi" -> Just $ ESpeed Hi
			_ -> Nothing
		, "unprepared" ~> finished EUnprepared
		]

	(c:cs) ~> f = (c, stripPrefix cs >=> f)
	finished v s = v <$ guard (null s)
	readField f = fmap f . readMaybe
	dropr c s = case reverse s of
		c':cs | c == c' -> Just (reverse cs)
		_ -> Nothing

	parseColor = \case
		'b' -> Just Blue
		'r' -> Just Red
		'y' -> Just Yellow
		_ -> Nothing
	
	parseCell 'd' = Just Empty
	parseCell c = Occupied (toEnum color) (toEnum shape)
		<$ guard (color <= colorBound && shape <= shapeBound)
		where
		w = fromEnum c
		color = (w .&. 0b00011) - 1
		shape = (w .&. 0b11100) `shiftR` 2
	colorBound = fromEnum (maxBound :: Color)
	shapeBound = fromEnum (maxBound :: Shape)
	
	parseOrientation = \case
		"horizontal" -> Just Horizontal
		"vertical" -> Just Vertical
		_ -> Nothing
