{-# LANGUAGE DeriveFunctor #-}

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Concurrent
import Control.Concurrent.STM
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
import Util
import qualified Data.HashMap.Strict as HM
import qualified Dr.Mario.Model as M

main :: IO ()
main = do
	gen <- createSystemRandom
	b <- randomBoard gen
	(c1, c2) <- randomPill gen
	params <- dmReroot (dmParameters gen b) [ChanceMove c1 c2]
	initialTree <- emptyTree params

	let c = Comms
	    	{ tree = initialTree
	    	, params = params
	    	, repetitions = Finite 0
	    	, sequenceNumber = minBound
	    	, iterations = 0
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
		, toggleIterations = 0
		}
	pure ()

data UIState = UIState
	{ comms :: TVar Comms
	, commsCache :: Comms
	, rootBoard :: M.Board
	, board :: M.Board
	, selectedMoves :: [MCMove]
	, nowTime :: UTCTime
	, toggleTime :: UTCTime
	, toggleIterations :: Double
	}

timerThread :: BChan () -> IO ()
timerThread chan = forever $ threadDelay 100000 >> writeBChan chan ()

app :: App UIState () ()
app = App
	{ appDraw = renderUIState
	, appChooseCursor = neverShowCursor
	, appHandleEvent = handleEvent
	, appStartEvent = pure
	, appAttrMap = const (attrMap defAttr [])
	}

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
				, toggleIterations = iterations (commsCache s)
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
		EvKey (KChar 'r') [] -> reroot s
		_ -> continue s
	_ -> continue s

modifyComms :: UIState -> (Comms -> Comms) -> EventM n (Next UIState)
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

reroot :: UIState -> EventM n (Next UIState)
reroot s = case focusTree ms (tree c) of
	Just (TreeFocus ts (Just i)) | visitCount (statistics tree') > 0 -> do
		params' <- liftIO $ dmReroot (params c) ms
		modifyComms s
			{ board = board'
			, rootBoard = board'
			, selectedMoves = []
			} $ \c -> c
			{ tree = tree'
			, params = params'
			, sequenceNumber = sequenceNumber c + 1
			}
		where
		(m, tree') = ts !! i
		board' = makeMove (board s) m
	_ -> continue s
	where
	c = commsCache s
	ms = selectedMoves s

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
	[ renderRollouts . iterations . commsCache $ s
	, hCenter $ renderRate
		(repetitions (commsCache s))
		(diffUTCTime (nowTime s) (toggleTime s))
		(iterations (commsCache s) - toggleIterations s)
	, renderBestMoves (board s) (focusStats (selectedMoves s) (tree (commsCache s)))
	]

renderStats :: MCStats -> Widget n
renderStats stats = vBox
	[ renderRollouts . visitCount $ stats
	, hCenter . str $ "value: " ++ (show5Float . meanUtility) stats
	]
	where
	show5Float v = showFFloat (Just 5) v ""

renderRollouts :: Double -> Widget n
renderRollouts n = hCenter . str $ (show . floor) n ++ " rollouts"

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
	$   renderLookaheadFor b l r
	<-> renderBoard b (const Nothing)
renderMove b (AIMove p) = raw
	-- if we put a blank line here, where the pill lookahead would go in the
	-- other case, the display doesn't appear to jump around vertically when
	-- navigating up and down the move tree
	$   char defAttr ' '
	<-> renderBoard b (pillOverlay p)
