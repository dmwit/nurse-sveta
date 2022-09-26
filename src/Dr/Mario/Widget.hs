{-# Language DataKinds #-} -- not on by default because of how much extra stuff it creates at the type/kind level

module Dr.Mario.Widget (
	DrawingGrid, dgNew, dgSetRenderer, dgWidget,
	dgSetSize, dgSetWidth, dgSetHeight,
	dgGetSize, dgGetWidth, dgGetHeight,

	PlayerStateModel(..),
	psmBoardL, psmLookaheadL, psmOverlayL,
	PlayerStateView, psvNew, psvWidget,
	psvGet, psvSet,
	psvModifyM, psvModifyM_, psvModify, psvModify_,
	) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.Monoid
import Dr.Mario.Model as DM
import GI.Cairo.Render
import GI.Cairo.Render.Connector
import GI.Gtk as G

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
