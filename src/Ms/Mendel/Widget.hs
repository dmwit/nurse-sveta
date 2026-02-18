module Ms.Mendel.Widget
	( module Ms.Mendel.Widget
	, module Nurse.Sveta.Widget
	) where

import GI.Cairo.Render.Connector
import GI.Gtk
import Ms.Mendel.Cairo
import Ms.Mendel.Population
import Nurse.Sveta.Cairo
import Nurse.Sveta.GameBrowser
import Nurse.Sveta.Util hiding (get)
import Nurse.Sveta.Widget

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified GI.Cairo.Render as C

-- * Variation tree

-- | Structure-only description of a move tree for layout.
data TreeLayout = TreeLayout
	{ tlMainLength :: Int
	, tlVariations :: [TreeLayout]
	} deriving (Eq, Ord, Read, Show)

treeLayoutFromMoveTree :: MoveTree a -> TreeLayout
treeLayoutFromMoveTree mt = TreeLayout
	{ tlMainLength = length (mainSequence mt)
	, tlVariations = map treeLayoutFromMoveTree (toList (variations mt))
	}

-- | Grid cell contents. Even columns: nodes or blank. Odd columns: edges or blank.
data GridCell
	= CellNode Bool  -- ^ True = root (ε), False = other (x)
	| CellEdge [EdgeComponent]
	deriving (Eq, Ord, Read, Show)

data EdgeComponent = LR | UD | LD | UR
	deriving (Eq, Ord, Read, Show)

data Rendering a = Rendering
	{ renderingWidth, renderingHeight :: Int
	, renderingTree :: GridPos -> Map GridPos a
	} deriving Functor

instance Default (Rendering a) where def = rempty 0 0
instance Show a => Show (Rendering a) where
	show r = printf "Rendering { renderingWidth = %d, renderingHeight = %d, renderingTree = %s }"
		(renderingWidth r)
		(renderingHeight r)
		(show (renderingTree r def))

combineRenderings :: Bool -> Bool -> Rendering a -> Rendering b -> Rendering (These a b)
combineRenderings overlapW overlapH as bs = Rendering
	{ renderingWidth = combineMetric overlapW (renderingWidth as) (renderingWidth bs)
	, renderingHeight = combineMetric overlapH (renderingHeight as) (renderingHeight bs)
	, renderingTree = \(xa, ya) -> let
		xb = offset overlapW xa (renderingWidth as)
		yb = offset overlapH ya (renderingHeight as)
		ta = renderingTree as (xa, ya)
		tb = renderingTree bs (xb, yb)
		in M.unions [M.intersectionWith These ta tb, This <$> ta, That <$> tb]
	}
	where
	combineMetric = \case False -> (+); True -> max
	offset = \case False -> (+); True -> const

noOverlap :: These a a -> a
noOverlap = these id id const

hcat, vcat :: Rendering a -> Rendering a -> Rendering a
hcat l r = noOverlap <$> combineRenderings False True l r
vcat u d = noOverlap <$> combineRenderings True False u d

hcats, vcats :: [Rendering a] -> Rendering a
hcats = foldb hcat def
vcats = foldb vcat def

hrep, vrep :: Int -> Rendering a -> Rendering a
hrep n r = hcats (replicate n r)
vrep n r = vcats (replicate n r)

overlap :: Semigroup a => Rendering a -> Rendering a -> Rendering a
overlap a b = these id id (<>) <$> combineRenderings True True a b

-- can't imagine this ever being useful, but the completionist in me needs to complete the set
kittyCorner :: Rendering a -> Rendering a -> Rendering a
kittyCorner ul br = noOverlap <$> combineRenderings False False ul br

rleaf :: a -> Rendering a
rleaf = Rendering 1 1 . flip M.singleton

rempty :: Int -> Int -> Rendering a
rempty w h = Rendering w h def

-- | Grid position (col, row). Even cols = nodes, odd = edges.
type GridPos = (Int, Int)

-- the top row is hard to click because you often hit the Paned hitbox instead
-- of the DrawingArea hitbox, so we leave a little space with the rempty 0 1 at
-- the start
buildGridFromMoveTree :: MoveTree a -> Rendering (GridCell, MoveTreeAddress)
buildGridFromMoveTree t0 = vcat (rempty 0 1) $ rleaf (CellNode True, def) `hcat` case length (mainSequence t0) of
	0 -> goVariations def (variations t0)
	_ -> rleaf (CellEdge [LR], MoveTreeAddress def 0) `hcat` goTree def t0
	where
	goVariations varPath vs = vcats . toList $ Seq.mapWithIndex (goVariation varPath (length vs)) vs
	goVariation varPath n i v
		| i == 0 = (edge [LR, LD] `vcat` verticalBar) `hcat` child
		| i == n - 1 = edge [UR] `hcat` child
		| otherwise = (edge [UD, UR] `vcat` verticalBar) `hcat` child
		where
		varPath' = varPath Seq.:|> i
		child = goTree varPath' v
		edge e = rleaf (CellEdge e, MoveTreeAddress varPath' 0)
		verticalBar = vrep (renderingHeight child - 1) (edge [UD])
	goTree varPath t = mainSeq `hcat` goVariations varPath (variations t) where
		ns = mainSequence t
		mainSeq = hcats . toList $ Seq.mapWithIndex (goNode varPath (length ns)) ns
	goNode varPath n i node
		| i == 0 = cell (CellNode False)
		| otherwise = cell (CellEdge [LR]) `hcat` cell (CellNode False)
		where cell c = rleaf (c, MoveTreeAddress varPath i)

cellSizePx :: Int
cellSizePx = 30

-- | Variation tree view: Cairo-based widget for rendering move trees.
-- Fixed 30-pixel cells, scrollable. Structured for future interactivity.
data VariationTreeView = VTV
	{ vtvCanvas :: DrawingArea
	, vtvAIButton :: CheckButton
	, vtvModel :: IORef (Map GridPos (GridCell, MoveTreeAddress))
	-- ^ (cells, nodeAddresses, minCol, minRow, colCount, rowCount)
	}

newVariationTreeView :: MonadIO m => m VariationTreeView
newVariationTreeView = do
	da <- new DrawingArea []
	ai <- new CheckButton []
	ref <- liftIO $ newIORef def
	drawingAreaSetDrawFunc da . Just $ \_ ctx _ _ -> do
		aiLol <- liftIO $ get ai #active
		model <- liftIO $ readIORef ref
		renderWithContext (vtvRender aiLol model) ctx
	on ai #toggled (#queueDraw da)
	pure (VTV da ai ref)

vtvWidget :: MonadIO m => VariationTreeView -> m Widget
vtvWidget = toWidget . vtvCanvas

vtvSet :: MonadIO m => VariationTreeView -> MoveTree a -> m ()
vtvSet vtv mt = do
	let grid = buildGridFromMoveTree mt
	    w = renderingWidth grid * cellSizePx
	    h = renderingHeight grid * cellSizePx
	liftIO $ writeIORef (vtvModel vtv) (renderingTree grid (0, 0))
	#setSizeRequest (vtvCanvas vtv) (fromIntegral w) (fromIntegral h)
	#queueDraw (vtvCanvas vtv)

-- | Install a callback for node clicks. Called with the MoveTreeAddress of the clicked node.
vtvOnNodeClick :: MonadIO m => VariationTreeView -> (MoveTreeAddress -> IO ()) -> m ()
vtvOnNodeClick vtv callback = do
	click <- new GestureClick []
	on click #pressed \_ nX nY -> do
		let col = floor (nX / fromIntegral cellSizePx)
		    row = floor (nY / fromIntegral cellSizePx)
		nodeAddrs <- liftIO $ readIORef (vtvModel vtv)
		for_ (M.lookup (col, row) nodeAddrs) (callback . snd)
	#addController (vtvCanvas vtv) click

vtvRender :: Bool -> Map GridPos (GridCell, MoveTreeAddress) -> C.Render ()
vtvRender aiLol cells = do
	C.setSourceRGB 0 0 0
	C.setLineWidth 0.08
	C.setLineCap C.LineCapRound
	C.setLineJoin C.LineJoinRound
	join C.scale (fromIntegral cellSizePx)
	-- we want to make the entire edge path before stroking so that we don't
	-- double-paint on the grid boundaries
	sequence_ $ flip M.mapWithKey edges \(x_, y_) components -> do
		let [x, y] = fromIntegral <$> [x_, y_]
		C.translate x y
		mapM_ (drawEdge aiLol) components
		C.translate (-x) (-y)
	C.stroke
	sequence_ $ flip M.mapWithKey nodes \(x, y) isRoot -> do
		fitText (fromIntegral x + 0.1) (fromIntegral y + 0.9) 0.8 (-0.8) (if isRoot then "ε" else "x")
	where
	(nodes, edges) = M.mapEither (\case (CellNode b, _) -> Left b; (CellEdge cs, _) -> Right cs) cells

-- aiLol: The first version of drawEdge was vibe coded. Below is an excerpt
-- from the prompt I wrote describing how I wanted things drawn. The AI I was
-- using sort of did what I said... but definitely didn't do what I meant. I
-- mean I would bet it basically understood what I meant and just got the
-- arguments to the arc functions a bit wrong, and I don't blame it, there's
-- like eight coordinate transforms going on and I would struggle to get it
-- right the first time, too. But it does look just hilarious, and I decided I
-- liked it enough to keep it around.
--
-- * No variations: line left to right
-- * Connecting the end of a main sequence to the first variation: line left to right and arc left to bottom
-- * Middle variations: line top to bottom and arc top to right
-- * Connecting variations: line top to bottom
-- * Final variation: arc top to right

-- | Draw edge in 1x1 cell. Screen coords (y down): left (0, 0.5), right (1, 0.5), top (0.5, 0), bottom (0.5, 1).
drawEdge :: Bool -> EdgeComponent -> C.Render ()
drawEdge aiLol = \case
	LR -> C.moveTo 0 0.5 >> C.lineTo 1 0.5
	UD -> C.moveTo 0.5 0 >> C.lineTo 0.5 1
	LD -> if aiLol
		then C.moveTo 1 0.5 >> C.arcNegative 0.5 0.5 0.5 pi (pi/2)
		else C.moveTo 0 0.5 >> C.arc 0 1 0.5 (3*pi/2) 0
	UR -> if aiLol
		then C.moveTo 0.5 0 >> C.arcNegative 0.5 0.5 0.5 (3*pi/2) 0
		else C.moveTo 1 0.5 >> C.arc 1 0 0.5 (pi/2) pi

data instance Pattern Gtk = PatternGtk
	{ pgCanvas :: DrawingGrid
	, pgReplication :: Replication CheckButton
	, pgContainer :: Box
	}

instance RepurposeIO Gtk Browsing Pattern where
	type instance RepurposingEnvironmentIO Gtk Browsing Pattern = ()
	repurposeIO _ pb = do
		let cs = pbConvolutionSize pb
		top <- new Box []
		canvas <- newDrawingGrid (fromIntegral (csWidth cs)) (fromIntegral (csHeight cs))
		canvasWidget <- dgWidget canvas
		mirroring <- new CheckButton [#label := "mirroring", #active := rMirroring (pbReplication pb), #sensitive := False]
		coloring <- new CheckButton [#label := "coloring", #active := rColoring (pbReplication pb), #sensitive := False]

		dgSetRenderer canvas (ptbRender (pbTemplate pb))
		#setSizeRequest canvasWidget (fromIntegral (csWidth cs * 80)) (fromIntegral (csHeight cs * 80))
		#append top canvasWidget
		#append top mirroring
		#append top coloring

		pure PatternGtk
			{ pgCanvas = canvas
			, pgReplication = Replication
				{ rMirroring = mirroring
				, rColoring = coloring
				}
			, pgContainer = top
			}

pgWidget :: Pattern Gtk -> IO Widget
pgWidget = toWidget . pgContainer
