import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.List
import Dr.Mario.Sveta
import Dr.Mario.Sveta.MCTS
import Graphics.Vty
import Numeric
import System.Random.MWC
import System.IO
import qualified Data.HashMap.Strict as HM
import qualified Dr.Mario.Model as M

main :: IO ()
main = do
	gen <- createSystemRandom
	level <- uniformR (0,20) gen
	board <- flip M.randomBoard level <$> uniformR (2, maxBound) gen
	c1 <- M.decodeColor <$> uniformR (2, maxBound) gen
	c2 <- M.decodeColor <$> uniformR (2, maxBound) gen
	params <- dmReroot (dmParameters gen board) (ChanceMove c1 c2)
	initialTree <- emptyTree params

	let c = Comms
	    	{ tree = initialTree
	    	, params = params
	    	, repetitions = Finite 0
	    	, sequenceNumber = minBound
	    	}
	commsRef <- newTVarIO c
	timerChan <- newBChan 1
	forkIO (mctsThread commsRef)
	forkIO (timerThread timerChan)

	customMain (mkVty defaultConfig) (Just timerChan) app UIState
		{ comms = commsRef
		, commsCache = c
		, tab = BestMoves
		, bestMovesState = BestMovesState
			{ bmsBoard = board
			}
		}
	pure ()

data Tab = BestMoves deriving (Bounded, Enum, Eq, Ord, Read, Show)

describeTab :: Tab -> String
describeTab BestMoves = "Best Moves"

data Repetitions = Finite Int | Infinity deriving (Eq, Ord, Read, Show)

toggleFiniteness :: Repetitions -> Repetitions
toggleFiniteness (Finite _) = Infinity
toggleFiniteness Infinity = Finite 0

increment :: Repetitions -> Repetitions
increment (Finite n) = Finite (n+1)
increment Infinity = Infinity

decrement :: Repetitions -> Repetitions
decrement (Finite n) = Finite (n-1)
decrement Infinity = Infinity

data Comms = Comms
	{ tree :: DrMarioTree
	, params :: DrMarioParameters
	, repetitions :: Repetitions
	, sequenceNumber :: Int
	}

data BestMovesState = BestMovesState
	{ bmsBoard :: M.Board
	} deriving (Eq, Ord, Read, Show)

data UIState = UIState
	{ comms :: TVar Comms
	, commsCache :: Comms
	, tab :: Tab
	, bestMovesState :: BestMovesState
	}

mctsThread :: TVar Comms -> IO ()
mctsThread commsRef = forever $ do
	c <- atomically $ do
		c <- readTVar commsRef
		guard (repetitions c > Finite 0)
		pure c
	t <- mcts (params c) (tree c)
	atomically $ do
		c' <- readTVar commsRef
		if sequenceNumber c == sequenceNumber c'
		then writeTVar commsRef c
			{ tree = t
			, repetitions = decrement (repetitions c)
			}
		else pure ()

timerThread :: BChan () -> IO ()
timerThread chan = forever $ threadDelay 100000 >> writeBChan chan ()

app :: App UIState () ()
app = App
	{ appDraw = renderUIState
	, appChooseCursor = neverShowCursor
	, appHandleEvent = handleEvent
	, appStartEvent = pure
	, appAttrMap = const theAttrMap
	}

theAttrMap :: AttrMap
theAttrMap = attrMap defAttr []

handleEvent :: UIState -> BrickEvent () () -> EventM () (Next UIState)
handleEvent s e = case e of
	AppEvent () -> modifyComms s id
	VtyEvent e' -> case e' of
		EvKey (KChar 'c') [MCtrl] -> halt s
		EvKey (KChar ' ') [] -> modifyComms s $ \c -> c
			{ repetitions = toggleFiniteness (repetitions c)
			, sequenceNumber = 1 + sequenceNumber c
			}
		EvKey (KChar 's') [] -> modifyComms s $ \c -> c
			{ repetitions = increment (repetitions c)
			, sequenceNumber = 1 + sequenceNumber c
			}
		_ -> continue s
	_ -> continue s

modifyComms :: UIState -> (Comms -> Comms) -> EventM () (Next UIState)
modifyComms s f = do
	c <- liftIO . atomically $ do
		c <- readTVar (comms s)
		let c' = f c
		writeTVar (comms s) c'
		pure c'
	continue s { commsCache = c }

renderUIState :: UIState -> [Widget ()]
renderUIState s = pure . joinBorders $ vBox
	[ renderTabs (tab s)
	, renderStats (statistics (tree (commsCache s)))
	, renderTab s
	]

renderTabs :: Tab -> Widget n
renderTabs t = id
	. hBox
	. intersperse (vLimit 3 vBorder)
	. map renderTabLabel
	$ [minBound .. maxBound]
	where
	renderTabLabel = vBox . sequence [highlight, hCenter . str . describeTab, highlight]
	highlight t' = if t == t' then hBorder else str " "

renderStats :: MCStats -> Widget n
renderStats stats = vBox
	[ hCenter . str $ (show . floor . visitCount) stats ++ " rollouts"
	, hCenter . str $ "value: " ++ show5Float averageValue
	]
	where
	show5Float v = showFFloat (Just 5) v ""
	averageValue = cumulativeUtility stats / visitCount stats

renderTab :: UIState -> Widget n
renderTab s = case tab s of
	BestMoves -> renderBestMoves (bestMovesState s) (tree (commsCache s))

renderBestMoves :: BestMovesState -> DrMarioTree -> Widget n
renderBestMoves bms t = vBox
	$  [ hCenter (str "(There are unexplored placements from this position.)") | _ <- take 1 (unexplored t) ]
	++ [ go ]
	where
	go = id
		. hBox
		. take 3
		. map (renderMoveAndStats (bmsBoard bms))
		. sortOn (negate . cumulativeUtility . statistics . snd)
		. HM.toList
		. children
		$ t

renderMoveAndStats :: M.Board -> (MCMove, MCTree MCStats MCMove) -> Widget n
renderMoveAndStats b (m, t) = vBox
	[ hCenter (renderMove b m)
	, renderStats (statistics t)
	]

renderMove :: M.Board -> MCMove -> Widget n
renderMove b (ChanceMove l r) = raw
	$   (string defAttr padding <|> renderCell (M.Occupied l M.West) <|> renderCell (M.Occupied r M.East))
	<-> renderBoard b (const Nothing)
	where padding = replicate (M.width b `div` 2 - 1) ' '
renderMove b (AIMove p) = raw (renderBoard b pillOverlay) where
	pillOverlay pos
		| pos == M.bottomLeftPosition p = Just (M.bottomLeftCell (M.content p))
		| pos == M.otherPosition p = Just (M.otherCell (M.content p))
		| otherwise = Nothing

renderBoard :: M.Board -> (M.Position -> Maybe M.Cell) -> Image
renderBoard b overlay = vertCat
	[ horizCat . intersperse (char defAttr ' ') $
		[ renderCell (fromMaybe (M.unsafeGet b pos) (overlay pos))
		| x <- [0 .. M.width b-1]
		, let pos = M.Position x y
		]
	| y <- [M.height b-1, M.height b-2 .. 0]
	]

renderCell :: M.Cell -> Image
renderCell M.Empty = char defAttr ' '
renderCell (M.Occupied c s) = char (colorAttr c) (shapeChar s)

colorAttr :: M.Color -> Attr
colorAttr c = defAttr `withForeColor` case c of
	M.Red    -> red
	M.Blue   -> cyan
	M.Yellow -> yellow

shapeChar :: M.Shape -> Char
shapeChar M.Virus        = '☻'
shapeChar M.Disconnected = 'o'
shapeChar M.North        = '^'
shapeChar M.South        = 'v'
shapeChar M.East         = '>'
shapeChar M.West         = '<'
