{-# Language DataKinds #-} -- not on by default because of how much extra stuff it creates at the type/kind level

module Dr.Mario.Widget (
	-- * Raw drawing grid
	DrawingGrid, dgNew, dgSetRenderer, dgWidget,
	dgSetSize, dgSetWidth, dgSetHeight,
	dgGetSize, dgGetWidth, dgGetHeight,

	-- * Player state
	PlayerStateModel(..),
	psmBoardL, psmLookaheadL, psmOverlayL,
	PlayerStateView, psvNew, psvWidget,
	psvGet, psvSet,
	psvModifyM, psvModifyM_, psvModify, psvModify_,

	-- * Search configuration
	SearchConfigurationView,
	scvNew, scvWidget, scvSet,

	-- * Thread management
	ThreadManager, ThreadView(..),
	tmNew, tmWidget, tmStartThread, tmDieThen,

	-- * Noticing when things change
	Stable,
	sNew,
	sPayload,
	sUpdate, sUpdateM, sSetPayload, sTrySetPayload,
	sOnSubterm,

	Tracker,
	tNew, tWhenUpdated,
	) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.List
import Data.Monoid
import Dr.Mario.Model as DM
import Dr.Mario.Tomcats (SearchConfiguration(..))
import GI.Cairo.Render
import GI.Cairo.Render.Connector
import GI.GLib
import GI.Gtk as G
import System.IO
import Text.Read

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified GHC.OverloadedLabels as Overload

data DrawingGrid = DG
	{ dgCanvas :: DrawingArea
	, dgFrame :: AspectFrame
	, dgSize :: IORef (Double, Double)
	}

-- | Takes a width and height. You get a 'DrawingArea'-alike thing where, when
-- you're drawing, you get to assume that the visible space ranges from 0-width
-- in the X direction (i.e. 0 is the far left of the widget, and width is the
-- far right of the widget -- so rendering a 1x1 thing starting at width-1 will
-- be fully visible and butt up against the edge) and 0-height in the Y
-- direction.
--
-- Uses the math convention for Y coordinates: bigger numbers are higher than
-- smaller numbers.
dgNew :: MonadIO m => Double -> Double -> m DrawingGrid
dgNew w h = do
	da <- new DrawingArea []
	af <- new AspectFrame $ tail [undefined
		, #xalign := 0.5
		, #yalign := 0.5
		, #ratio := realToFrac (w / h)
		, #child := da
		, #obeyChild := False
		, #hexpand := True
		, #vexpand := True
		]
	ref <- liftIO $ newIORef (w, h)
	pure (DG da af ref)

-- TODO: would be nice to plug into gi's attribute mechanism instead of having
-- explicit getters and setters like this... especially if this means we could
-- *also* plug into the mechanism for `new`
dgSetSize :: MonadIO m => DrawingGrid -> Double -> Double -> m ()
dgSetSize dg w h = do
	set (dgFrame dg) [#ratio := realToFrac (w / h)]
	liftIO $ writeIORef (dgSize dg) (w, h)
	#queueDraw dg

dgSetWidth :: MonadIO m => DrawingGrid -> Double -> m ()
dgSetWidth dg w = do
	(_, h) <- liftIO $ readIORef (dgSize dg)
	dgSetSize dg w h

dgSetHeight :: MonadIO m => DrawingGrid -> Double -> m ()
dgSetHeight dg h = do
	(w, _) <- liftIO $ readIORef (dgSize dg)
	dgSetSize dg w h

dgGetSize :: MonadIO m => DrawingGrid -> m (Double, Double)
dgGetSize = liftIO . readIORef . dgSize

dgGetWidth :: MonadIO m => DrawingGrid -> m Double
dgGetWidth = fmap fst . dgGetSize

dgGetHeight :: MonadIO m => DrawingGrid -> m Double
dgGetHeight = fmap snd . dgGetSize

dgSetRenderer :: MonadIO m => DrawingGrid -> Render () -> m ()
dgSetRenderer dg draw = drawingAreaSetDrawFunc (dgCanvas dg) . Just $ \_ ctx _ _ -> flip renderWithContext ctx $ do
	wCanvas <- #getWidth (dgCanvas dg)
	hCanvas <- #getHeight (dgCanvas dg)
	(wGrid, hGrid) <- dgGetSize dg
	scale (fromIntegral wCanvas / wGrid) (-fromIntegral hCanvas / hGrid)
	translate 0 (-hGrid)
	draw

dgWidget :: MonadIO m => DrawingGrid -> m Widget
dgWidget = toWidget . dgFrame

instance (MonadIO m, a ~ m ()) => Overload.IsLabel "queueDraw" (DrawingGrid -> a) where fromLabel = #queueDraw . dgCanvas

data PlayerStateModel = PSM
	{ psmBoard :: Board
	, psmLookahead :: Maybe (Color, Color)
	, psmOverlay :: [(Pill, Double)] -- ^ the 'Double's are opacities, 1 for fully visible, 0 for fully transparent
	} deriving (Eq, Ord, Read, Show)

psmBoardL :: PlayerStateModel -> (Board, Board -> PlayerStateModel)
psmBoardL psm = (psmBoard psm, \b -> psm { psmBoard = b })

psmLookaheadL :: PlayerStateModel -> (Maybe (Color, Color), Maybe (Color, Color) -> PlayerStateModel)
psmLookaheadL psm = (psmLookahead psm, \l -> psm { psmLookahead = l })

psmOverlayL :: PlayerStateModel -> ([(Pill, Double)], [(Pill, Double)] -> PlayerStateModel)
psmOverlayL psm = (psmOverlay psm, \o -> psm { psmOverlay = o })

psmRender :: PlayerStateModel -> Render ()
psmRender psm = do
	setSourceRGB 0 0 0
	setLineWidth 0.4
	setLineCap LineCapRound
	setLineJoin LineJoinRound
	centered moveTo (head bottleCoordinates)
	mapM_ (centered lineTo) (tail bottleCoordinates)
	stroke
	for_ (psmLookahead psm) $ \(l, r) -> do
		renderCell xMid (h+2) (Occupied l West)
		renderCell (xMid+1) (h+2) (Occupied r East)
	getAp $ ofoldMapWithKey (\(Position x y) -> Ap . renderCell (fromIntegral x) (fromIntegral y)) b
	for_ (psmOverlay psm) $ \(pill, opacity) -> do
		pushGroup
		renderPill pill
		popGroupToSource
		paintWithAlpha opacity
	where
	b = psmBoard psm
	xMid = fromIntegral $ (DM.width b-1) `quot` 2
	w = fromIntegral . DM.width $ b
	h = fromIntegral . DM.height $ b

	bottleCoordinates = tail [undefined
		, (xMid-1, h+3)
		, (xMid-1, h+2)
		, (xMid, h+2)
		, (xMid, h+1)
		, (0, h+1)
		, (0, 0)
		, (w+1, 0)
		, (w+1, h+1)
		, (xMid+3, h+1)
		, (xMid+3, h+2)
		, (xMid+4, h+2)
		, (xMid+4, h+3)
		]

data PlayerStateView = PSV
	{ psvCanvas :: DrawingGrid
	, psvModel :: IORef PlayerStateModel
	}

psvNew :: MonadIO m => PlayerStateModel -> m PlayerStateView
psvNew psm = do
	dg <- dgNew (fromIntegral (DM.width b + 2)) (fromIntegral (DM.height b + 4))
	ref <- liftIO $ newIORef psm
	dgSetRenderer dg $ liftIO (readIORef ref) >>= psmRender
	psvUpdateHeightRequest dg psm
	pure (PSV dg ref)
	where b = psmBoard psm

psvUpdateHeightRequest :: MonadIO m => DrawingGrid -> PlayerStateModel -> m ()
psvUpdateHeightRequest dg psm = do
	w <- dgWidget dg
	set w [#heightRequest := fromIntegral (4 * DM.height (psmBoard psm))]

psvWidget :: MonadIO m => PlayerStateView -> m Widget
psvWidget = dgWidget . psvCanvas

psvGet :: MonadIO m => PlayerStateView -> m PlayerStateModel
psvGet = liftIO . readIORef . psvModel

psvSet :: MonadIO m => PlayerStateView -> PlayerStateModel -> m ()
psvSet psv psm = do
	liftIO $ writeIORef (psvModel psv) psm
	psvUpdateHeightRequest (psvCanvas psv) psm
	#queueDraw psv

psvModifyM :: MonadIO m => PlayerStateView -> (PlayerStateModel -> m (PlayerStateModel, a)) -> m a
psvModifyM psv f = do
	psm <- psvGet psv
	(psm', a) <- f psm
	a <$ psvSet psv psm'

psvModifyM_ :: MonadIO m => PlayerStateView -> (PlayerStateModel -> m PlayerStateModel) -> m ()
psvModifyM_ psv f = psvModifyM psv (fmap (flip (,) ()) . f)

psvModify :: MonadIO m => PlayerStateView -> (PlayerStateModel -> (PlayerStateModel, a)) -> m a
psvModify psv f = psvModifyM psv (pure . f)

psvModify_ :: MonadIO m => PlayerStateView -> (PlayerStateModel -> PlayerStateModel) -> m ()
psvModify_ psv f = do
	liftIO $ modifyIORef (psvModel psv) f
	#queueDraw psv

instance (MonadIO m, a ~ m ()) => Overload.IsLabel "queueDraw" (PlayerStateView -> a) where fromLabel = #queueDraw . psvCanvas

renderPill :: Pill -> Render ()
renderPill Pill
	{ content = PillContent
		{ orientation = dir
		, bottomLeftColor = bl
		, otherColor = o
		}
	, bottomLeftPosition = Position (fromIntegral -> x) (fromIntegral -> y)
	} = case dir of
		Horizontal -> renderCell  x     y    (Occupied bl West )
		           >> renderCell (x+1)  y    (Occupied o  East )
		Vertical   -> renderCell  x     y    (Occupied bl South)
		           >> renderCell  x    (y+1) (Occupied o  North)

renderCell :: Double -> Double -> Cell -> Render ()
renderCell _ _ Empty = pure ()
renderCell x_ y_ (Occupied color shape) = do
	case shape of
		Virus -> do
			centered moveTo pos
			centered arc pos 0.4 0 (2*pi)
			closePath
		Disconnected -> do
			centered moveTo pos
			centered arc pos 0.3 0 (2*pi)
			closePath
		North -> do
			moveTo (x+0.8) (y+0.5)
			centered arc pos 0.3 0 pi
			lineTo (x+0.2) (y+0.1)
			lineTo (x+0.8) (y+0.1)
			closePath
		South -> do
			moveTo (x+0.2) (y+0.5)
			centered arc pos 0.3 pi (2*pi)
			lineTo (x+0.8) (y+0.9)
			lineTo (x+0.2) (y+0.9)
			closePath
		West -> do
			moveTo (x+0.5) (y+0.8)
			centered arc pos 0.3 (pi/2) (3*pi/2)
			lineTo (x+0.9) (y+0.2)
			lineTo (x+0.9) (y+0.8)
			closePath
		East -> do
			moveTo (x+0.5) (y+0.2)
			centered arc pos 0.3 (3*pi/2) (5*pi/2)
			lineTo (x+0.1) (y+0.8)
			lineTo (x+0.1) (y+0.2)
			closePath

	setSourceRGB 0 0 0
	setLineWidth 0.05
	strokePreserve
	renderColor color
	fill

	when (shape == Virus) $ do
		setSourceRGB 0 0 0
		moveTo (x+0.35) (y+0.65)
		arc (x+0.35) (y+0.65) 0.1 0 (2*pi)
		moveTo (x+0.65) (y+0.65)
		arc (x+0.65) (y+0.65) 0.1 0 (2*pi)
		fill
		setLineWidth 0.1
		moveTo (x+0.35) (y+0.35)
		lineTo (x+0.65) (y+0.35)
		stroke

	where
	x = x_ + 1
	y = y_ + 1
	pos = (x, y)

renderColor :: Color -> Render ()
renderColor = \case
	Blue -> setSourceRGB 0.13 0.49 0.72
	Red -> setSourceRGB 0.99 0.39 0.41
	Yellow -> setSourceRGB 0.82 0.79 0.26

centered :: (Double -> Double -> a) -> (Double, Double) -> a
centered f (x,y) = f (x+0.5) (y+0.5)

data SearchConfigurationView = SCV
	{ scvTop :: Grid
	, scvCache :: IORef (SearchConfiguration, SearchConfiguration)
	, scvC_puct :: Label
	, scvIterations :: Label
	}

scvNew :: SearchConfiguration -> (SearchConfiguration -> IO ()) -> IO SearchConfigurationView
scvNew sc request = do
	grid <- new Grid [#columnSpacing := 7, #rowSpacing := 3]
	cache <- newIORef (sc, sc)

	let mkLabel t = new Label [#label := t, #halign := AlignStart]

	c_puctLabel <- new Label [#halign := AlignStart]
	#setMarkup c_puctLabel "c<sub>puct</sub>"
	c_puctEditor <- new Entry [#inputPurpose := InputPurposeNumber]
	c_puctBuffer <- G.get c_puctEditor #buffer
	let c_puctText = tshow (c_puct sc)
	set c_puctBuffer [#text := c_puctText]
	c_puctDisplay <- mkLabel c_puctText

	iterationsLabel <- mkLabel "iterations per move"
	iterationsEditor <- new Entry [#inputPurpose := InputPurposeDigits]
	iterationsBuffer <- G.get iterationsEditor #buffer
	let iterationsText = tshow (iterations sc)
	set iterationsBuffer [#text := iterationsText]
	iterationsDisplay <- mkLabel iterationsText

	let scv = SCV
	    	{ scvTop = grid
	    	, scvCache = cache
	    	, scvC_puct = c_puctDisplay
	    	, scvIterations = iterationsDisplay
	    	}

	on c_puctEditor #activate $ do
		c_puctText <- G.get c_puctBuffer #text
		for_ (tread c_puctText) $ \c -> do
			(scReq, scCur) <- readIORef cache
			let scReq' = scReq { c_puct = c }
			request scReq'
			writeIORef cache (scReq', scCur)
			scvDisplay scv scReq' scCur

	on iterationsEditor #activate $ do
		iterationsText <- G.get iterationsBuffer #text
		for_ (tread iterationsText) $ \n -> do
			(scReq, scCur) <- readIORef cache
			let scReq' = scReq { iterations = n }
			request scReq'
			writeIORef cache (scReq', scCur)
			scvDisplay scv scReq' scCur

	#attach grid c_puctLabel   0 0 1 1
	#attach grid c_puctEditor  1 0 1 1
	#attach grid c_puctDisplay 2 0 1 1

	#attach grid iterationsLabel   0 1 1 1
	#attach grid iterationsEditor  1 1 1 1
	#attach grid iterationsDisplay 2 1 1 1

	pure scv

scvWidget :: SearchConfigurationView -> IO Widget
scvWidget = toWidget . scvTop

scvSet :: SearchConfigurationView -> SearchConfiguration -> IO ()
scvSet scv scCur = do
	(scReq, _) <- readIORef (scvCache scv)
	writeIORef (scvCache scv) (scReq, scCur)
	scvDisplay scv scReq scCur

-- | If you see this documentation, please report a bug.
scvDisplay :: SearchConfigurationView -> SearchConfiguration -> SearchConfiguration -> IO ()
scvDisplay scv scReq scCur = do
	set (scvC_puct scv) [#label := describe c_puct]
	set (scvIterations scv) [#label := describe iterations]
	where
	describe :: (Eq a, Show a) => (SearchConfiguration -> a) -> T.Text
	describe f = tshow cur <> if req == cur then mempty else " (will change to " <> tshow req <> " next game)"
		where
		req = f scReq
		cur = f scCur

data ThreadManager = ThreadManager
	{ tmContainer :: Box
	, tmThreadList :: Box
	, tmAddThread :: Button
	, tmRunningThreads :: IORef [Button]
	, tmDying :: IORef (Maybe (IO ()))
	, tmThreadDescription :: T.Text
	, tmFactory :: IO ThreadView
	}

data ThreadView = ThreadView
	{ tvWidget :: Widget
	, tvRefresh :: IO ()
	, tvCompute :: IO () -> IO ()
	}

data ThreadStatus = Live | Dying | Dead SomeException deriving Show

data DiedSuccessfully = DiedSuccessfully deriving (Eq, Ord, Read, Show, Bounded, Enum)
instance Exception DiedSuccessfully where displayException _ = "died successfully"

tmNew :: T.Text -> IO ThreadView -> IO ThreadManager
tmNew nm mkView = do
	top <- new Box [#orientation := OrientationVertical]
	lst <- new Box [#orientation := OrientationVertical]
	scr <- new ScrolledWindow [#child := lst]
	lbl <- new Label [#label := nm <> " threads"]
	add <- new Button $ tail [undefined
		, #iconName := "list-add"
		, #halign := AlignCenter
		]
	run <- newIORef []
	die <- newIORef Nothing
	let tm = ThreadManager top lst add run die nm mkView

	#append top lbl
	#append top add
	#append top scr
	on add #clicked (tmStartThread tm)
	pure tm

tmWidget :: ThreadManager -> IO Widget
tmWidget = toWidget . tmContainer

tmDieThen :: ThreadManager -> IO () -> IO ()
tmDieThen tm report = do
	set (tmAddThread tm) [#sensitive := False]
	writeIORef (tmDying tm) (Just report)

	btns <- readIORef (tmRunningThreads tm)
	traverse_ #activate btns
	when (null btns) report

-- ╭╴top╶───────────╮
-- │╭╴sta╶─────────╮│
-- ││╭╴btn╶╮╭╴lbl╶╮││
-- ││╰─────╯╰─────╯││
-- │╰──────────────╯│
-- │╭╴tvw╶─────────╮│
-- │╰──────────────╯│
-- ╰────────────────╯
tmStartThread :: ThreadManager -> IO ()
tmStartThread tm = readIORef (tmDying tm) >>= \case
	Just{} -> T.hPutStrLn stderr $ "WARNING: requested the creation of a new " <> tmThreadDescription tm <> " thread after beginning the shutdown process; ignoring request"
	Nothing -> do
		tv <- tmFactory tm

		top <- new Box [#orientation := OrientationVertical]
		sta <- new Box [#orientation := OrientationHorizontal]
		btn <- new Button []
		lbl <- new Label []

		tsRef <- newMVar (sNew Live)
		staTracker <- tNew
		let updateDisplay ts = do
		    	tWhenUpdated staTracker ts $ \s -> do
		    		set lbl [#label := tshow s]
		    		case s of
		    			Live -> set btn [#iconName := "process-stop", #sensitive := True]
		    			Dying -> set btn [#iconName := "edit-delete", #sensitive := False]
		    			Dead{} -> set btn [#iconName := "edit-delete", #sensitive := True]
		    	tvRefresh tv
		readMVar tsRef >>= updateDisplay

		#append sta btn
		#append sta lbl
		#append top sta
		#append top (tvWidget tv)
		#append (tmThreadList tm) top

		on btn #clicked $ do
			set btn [#sensitive := False]
			ts <- takeMVar tsRef
			case sPayload ts of
				Dead{} -> #remove (tmThreadList tm) top
				_ -> do
					putMVar tsRef (sSetPayload Dying ts)
					updateDisplay ts

		timeoutAdd PRIORITY_DEFAULT 30 $ do
			ts <- readMVar tsRef
			updateDisplay ts
			case sPayload ts of
				Dead{} -> do
					btns <- delete btn <$> readIORef (tmRunningThreads tm)
					writeIORef (tmRunningThreads tm) btns
					when (null btns) (readIORef (tmDying tm) >>= sequence_)
					pure SOURCE_REMOVE
				_ -> pure SOURCE_CONTINUE

		modifyIORef (tmRunningThreads tm) (btn:)
		() <$ forkIO (catch
			(tvCompute tv (tmCheckStatus tsRef))
			(\e -> modifyMVar_ tsRef (pure . sSetPayload (Dead e)))
			)

tmCheckStatus :: MVar (Stable ThreadStatus) -> IO ()
tmCheckStatus tsRef = do
	ts <- readMVar tsRef
	case sPayload ts of
		Dying -> throwIO DiedSuccessfully
		_ -> pure ()

-- | A type for tracking the updates to something that doesn't change very often.
data Stable a = Stable
	-- this generation is in the sense of epoch/number of steps/etc.
	{ generation :: {-# UNPACK #-} !Int
	, sPayload_ :: a
	} deriving (Eq, Ord, Read, Show)

sNew :: a -> Stable a
sNew = Stable (minBound+1)

sPayload :: Stable a -> a
sPayload = sPayload_

sUpdate :: (a -> a) -> Stable a -> Stable a
sUpdate f (Stable g p) = Stable (g + 1) (f p)

sUpdateM :: (a -> Maybe a) -> Stable a -> Stable a
sUpdateM f s@(Stable g p) = case f p of
	Nothing -> s
	Just p' -> Stable (g+1) p'

sSetPayload :: a -> Stable a -> Stable a
sSetPayload p s = Stable (generation s + 1) p

sTrySetPayload :: Eq a => a -> Stable a -> Stable a
sTrySetPayload p s = if p == sPayload s then s else Stable (generation s + 1) p

sOnSubterm :: (a -> (b, b -> a)) -> (Stable b -> Stable b) -> Stable a -> Stable a
sOnSubterm lens f sa@(Stable g pa) = if g == g' then sa else Stable g' pa'
	where
	(pb, mk) = lens pa
	Stable g' pb' = f (Stable g pb)
	pa' = mk pb'

-- | Not thread safe.
newtype Tracker = Tracker { generationCacheRef :: IORef Int }

tNew :: IO Tracker
tNew = Tracker <$> newIORef minBound

tWhenUpdated :: Tracker -> Stable a -> (a -> IO b) -> IO ()
tWhenUpdated t s f = do
	oldGen <- readIORef (generationCacheRef t)
	when (newGen > oldGen) $ do
		f (sPayload s)
		writeIORef (generationCacheRef t) newGen
	where
	newGen = generation s

tshow :: Show a => a -> T.Text
tshow = T.pack . show

tread :: Read a => T.Text -> Maybe a
tread = readMaybe . T.unpack
