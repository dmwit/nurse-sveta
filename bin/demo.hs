{-# LANGUAGE DeriveFunctor #-}

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.List
import Data.Time
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
	b <- flip M.randomBoard level <$> uniformR (2, maxBound) gen
	c1 <- M.decodeColor <$> uniformR (2, maxBound) gen
	c2 <- M.decodeColor <$> uniformR (2, maxBound) gen
	params <- dmReroot (dmParameters gen b) (ChanceMove c1 c2)
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

	now <- getCurrentTime
	customMain (mkVty defaultConfig) (Just timerChan) app UIState
		{ comms = commsRef
		, commsCache = c
		, rootBoard = b
		, board = b
		, selectedMoves = []
		, nowTime = now
		, toggleTime = now
		, toggleVisitCount = 0
		}
	pure ()

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

data UIState = UIState
	{ comms :: TVar Comms
	, commsCache :: Comms
	, rootBoard :: M.Board
	, board :: M.Board
	, selectedMoves :: [MCMove]
	, nowTime :: UTCTime
	, toggleTime :: UTCTime
	, toggleVisitCount :: Double
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
	AppEvent () -> do
		now <- liftIO getCurrentTime
		modifyComms s { nowTime = now } id
	VtyEvent e' -> case e' of
		EvKey (KChar 'c') [MCtrl] -> halt s
		EvKey (KChar ' ') [] -> do
			now <- liftIO getCurrentTime
			modifyComms s
				{ nowTime = now
				, toggleTime = now
				, toggleVisitCount = (visitCount . statistics . tree . commsCache) s
				} $ \c -> c
				{ repetitions = toggleFiniteness (repetitions c)
				, sequenceNumber = 1 + sequenceNumber c
				}
		EvKey (KChar 's') [] -> modifyComms s $ \c -> c
			{ repetitions = increment (repetitions c)
			, sequenceNumber = 1 + sequenceNumber c
			}
		EvKey KLeft [] -> changeFocus KLeft s
		EvKey KRight [] -> changeFocus KRight s
		EvKey KDown [] -> changeFocus KDown s
		EvKey KUp [] -> changeFocus KUp s
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

data TreeFocus a = TreeFocus
	{ bestMoves :: [(MCMove, a)]
	, selectedMoveIndex :: Maybe Int
	} deriving (Eq, Ord, Read, Show, Functor)

bestMovesFor :: DrMarioTree -> [(MCMove, DrMarioTree)]
bestMovesFor t = exploredMoves ++ unexploredMoves where
	exploredMoves = sortOn (negate . meanUtility . statistics . snd) (HM.toList (children t))
	unexploredMoves = [(m, MCTree mempty mempty mempty) | m <- unexplored t]

focusTree :: [MCMove] -> DrMarioTree -> Maybe (TreeFocus DrMarioTree)
focusTree [] t = Just TreeFocus
	{ bestMoves = bestMovesFor t
	, selectedMoveIndex = Nothing
	}
focusTree [m] t = Just TreeFocus
	{ bestMoves = ms
	, selectedMoveIndex = findIndex ((m==) . fst) ms
	} where ms = bestMovesFor t
focusTree (m:ms) t = HM.lookup m (children t) >>= focusTree ms

focusStats :: [MCMove] -> DrMarioTree -> Maybe (TreeFocus MCStats)
focusStats ms t = fmap (fmap statistics) (focusTree ms t)

changeFocus :: Key -> UIState -> EventM n (Next UIState)
changeFocus key s = continue $ case (key, focusTree (selectedMoves s) (tree (commsCache s))) of
	(KUp, Nothing) -> findValidMovePrefix s
	(KUp, Just ft) -> refreshBoard s { selectedMoves = safeInit (selectedMoves s) }
	(_, Just (TreeFocus ((m,_):_) Nothing)) -> replaceLastMove s m
	(_, Just (TreeFocus ms (Just i))) -> case key of
		KLeft  -> replaceLastMove s $ fst (ms !! max (i-1) 0)
		KRight -> replaceLastMove s $ fst (ms !! min (i+1) (length ms-1))
		KDown  -> case bestMovesFor t of
			(m',_):_ -> s
				{ selectedMoves = selectedMoves s ++ [m']
				, board = makeMove (board s) m
				}
			_ -> s
			where (m, t) = ms !! i
		_ -> s
	_ -> s

safeInit :: [a] -> [a]
safeInit xs = zipWith const xs (drop 1 xs)

replaceLastMove :: UIState -> MCMove -> UIState
replaceLastMove s m = s { selectedMoves = safeInit (selectedMoves s) ++ [m] }

findValidMovePrefix :: UIState -> UIState
findValidMovePrefix s = refreshBoard s { selectedMoves = go (selectedMoves s) (tree (commsCache s)) } where
	go [] _ = []
	go (m:ms) t = case HM.lookup m (children t) of
		Nothing -> []
		Just t' -> m : go ms t'

refreshBoard :: UIState -> UIState
refreshBoard s = s { board = go (rootBoard s) (selectedMoves s) } where
	go b [] = b
	go b [m] = b
	go b (m:ms) = go (makeMove b m) ms

makeMove :: M.Board -> MCMove -> M.Board
makeMove b m = case m of
	ChanceMove _ _ -> b
	AIMove p -> case M.place b p of
		Just (_, b') -> b'
		Nothing -> error
			$ "The impossible happened: the AI chose the illegal move "
			++ show p
			++ " on this board:\n"
			++ M.pp b

renderUIState :: UIState -> [Widget ()]
renderUIState s = pure . joinBorders $ vBox
	[ renderStats (statistics (tree (commsCache s)))
	, hCenter $ renderRate
		(repetitions (commsCache s))
		(diffUTCTime (nowTime s) (toggleTime s))
		((visitCount . statistics . tree . commsCache) s - toggleVisitCount s)
	, renderBestMoves (board s) (focusStats (selectedMoves s) (tree (commsCache s)))
	]

renderStats :: MCStats -> Widget n
renderStats stats = vBox
	[ hCenter . str $ (show . floor . visitCount) stats ++ " rollouts"
	, hCenter . str $ "value: " ++ (show5Float . meanUtility) stats
	]
	where
	show5Float v = showFFloat (Just 5) v ""

renderRate :: Repetitions -> NominalDiffTime -> Double -> Widget n
renderRate (Finite _) _ _ = str "Currently in single-stepping mode."
renderRate Infinity t_ c = str
	. showFFloat (Just 0) c
	. (" rollouts in "++)
	. showFFloat (Just 2) t
	. (" seconds = "++)
	. showFFloat (Just 2) (c/t)
	$ " rps"
	where
	t = realToFrac t_

renderBestMoves :: M.Board -> Maybe (TreeFocus MCStats) -> Widget n
renderBestMoves b Nothing = hCenter (str "The path you were looking at has been invalidated.")
renderBestMoves b (Just (TreeFocus ms mi)) = id
	. hBox
	. take movesToShow
	. zipWith3 (renderMoveAndStats b) ranks focuses
	. drop startingIndex
	$ ms
	where
	len = length ms
	startingIndex = case mi of
		Nothing -> 0
		Just i -> max 0 (min (len-movesToShow) (i - movesToShow`div`2))
	focuses = case mi of
		Nothing -> repeat False
		Just i -> replicate (i-startingIndex) False ++ [True] ++ repeat False
	ranks = [startingIndex+1 ..]

movesToShow :: Int
movesToShow = 5

renderMoveAndStats :: M.Board -> Int -> Bool -> (MCMove, MCStats) -> Widget n
renderMoveAndStats b rank focused (m, stats) = withBorderStyle style . border $ vBox
	[ hCenter (renderMove b m)
	, renderStats stats
	, hCenter (str ("rank " ++ show rank))
	] where
	style = if focused then unicode else borderStyleFromChar ' '

renderMove :: M.Board -> MCMove -> Widget n
renderMove b (ChanceMove l r) = raw
	$   (string defAttr padding <|> renderCell (M.Occupied l M.West) <|> char defAttr ' ' <|> renderCell (M.Occupied r M.East))
	<-> renderBoard b (const Nothing)
	where padding = replicate (2*(M.width b `div` 2 - 1)) ' '
renderMove b (AIMove p) = raw
	-- if we put a blank line here, where the pill lookahead would go in the
	-- other case, the display doesn't appear to jump around vertically when
	-- navigating up and down the move tree
	$   char defAttr ' '
	<-> renderBoard b pillOverlay
	where
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
