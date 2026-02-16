module Ms.Mendel.Widget
	( module Ms.Mendel.Widget
	, module Nurse.Sveta.Widget
	) where

import qualified GI.Cairo.Render as C
import GI.Gtk
import Ms.Mendel.Cairo
import Ms.Mendel.Population
import Nurse.Sveta.Cairo (fitText)
import Nurse.Sveta.Util
import Nurse.Sveta.Widget

-- * Variation tree

-- | Structure-only description of a move tree for layout. Convert from
-- 'Nurse.Sveta.GameBrowser.MoveTree' with 'treeLayoutFromMoveTree'.
data TreeLayout = TreeLayout
	{ tlMainLength :: Int
	, tlVariations :: [TreeLayout]
	} deriving (Eq, Ord, Read, Show)

treeLayoutFromMoveTree :: Foldable f => f a -> [TreeLayout] -> TreeLayout
treeLayoutFromMoveTree ms vars = TreeLayout
	{ tlMainLength = length ms
	, tlVariations = vars
	}

-- | Grid cell contents. Even columns: nodes or blank. Odd columns: edges or blank.
data GridCell
	= CellBlank
	| CellNode Bool  -- ^ True = root (ε), False = other (x)
	| CellEdge EdgeKind
	deriving (Eq, Ord, Read, Show)

data EdgeKind
	= EdgeStraight           -- ^ No variations: line left to right
	| EdgeMainToFirst        -- ^ Main line to first variation: line left-right + arc left-bottom
	| EdgeMiddleVariation    -- ^ Middle variation: line top-bottom + arc top-right
	| EdgeConnectingVariation -- ^ Connecting variations: line top to bottom
	| EdgeFinalVariation     -- ^ Final variation: arc top to right only
	deriving (Eq, Ord, Read, Show)

-- | Grid position (col, row). Even cols = nodes, odd = edges.
type GridPos = (Int, Int)

variationRows :: TreeLayout -> Int
variationRows (TreeLayout _ vars) = 1 + sum (map variationRows vars)

-- | Build the grid from a tree layout. Returns list of (position, cell).
-- Row 0 = main line. Variations stacked in rows 1+.
buildGrid :: TreeLayout -> [(GridPos, GridCell)]
buildGrid = go 0 0
	where
	go col row (TreeLayout n vars) = mainNodes ++ mainEdges ++ variationCells
		where
		-- Main: n+1 nodes at (col, row), (col+2, row), ..., (col+2*n, row)
		-- n edges at (col+1, row), ..., (col+2*n-1, row)
		mainNodes = [ ((col + 2*i, row), CellNode (i == 0)) | i <- [0..n] ]
		mainEdges = case vars of
			[] -> [ ((col + 2*i + 1, row), CellEdge EdgeStraight) | i <- [0..n-1] ]
			_ -> let
				straightEdges = [ ((col + 2*i + 1, row), CellEdge EdgeStraight) | i <- [0..n-2] ]
				junctionEdgeCol = col + max 1 (2*n - 1)
				junctionEdge = ((junctionEdgeCol, row), CellEdge EdgeMainToFirst)
				in straightEdges ++ [junctionEdge]
		-- Variation stem column: (col + 2*n - 1, 1), (col + 2*n - 1, 2), ...
		-- Variation nodes: (col + 2*n - 2, row) - same column as junction node
		-- Junction node at (col + 2*n - 2, 0) when n>=1. Variations at (col + 2*n - 2, r).
		junctionCol = col + 2 * max 0 (n - 1)
		stemCol = col + max 1 (2*n - 1)
		variationCells = case vars of
			[] -> []
			_ -> let
				rowStarts = scanl (+) 1 (map variationRows vars)
				stemRows = [1 .. last rowStarts - 1]
				stemCells = [ ((stemCol, r), kindFor r) | r <- stemRows ]
				kindFor r
					| r == 1 = EdgeMiddleVariation  -- first variation: stem from above + arc right
					| r == last rowStarts - 1 = EdgeFinalVariation  -- last: arc only
					| otherwise = EdgeConnectingVariation
				stemCellList = map (\(pos, k) -> (pos, CellEdge k)) stemCells
				varCells = concatMap (\(r0, v) -> go junctionCol r0 v) $
					zip rowStarts vars
				in stemCellList ++ varCells

-- | Bounding box of the grid: (minCol, minRow, maxCol+1, maxRow+1)
gridBounds :: [(GridPos, GridCell)] -> (Int, Int, Int, Int)
gridBounds cells
	| null cells = (0, 0, 1, 1)
	| otherwise = (minimum cols, minimum rows, maximum cols + 1, maximum rows + 1)
	where
	(cols, rows) = unzip [ (c, r) | ((c, r), _) <- cells ]

-- | Variation tree view: Cairo-based widget for rendering move trees.
-- Structured for future interactivity (hit testing, selection).
data VariationTreeView = VTV
	{ vtvCanvas :: DrawingGrid
	, vtvModel :: IORef [(GridPos, GridCell)]
	}

newVariationTreeView :: MonadIO m => TreeLayout -> m VariationTreeView
newVariationTreeView tl = do
	let cells = buildGrid tl
	    (minC, minR, maxC, maxR) = gridBounds cells
	    w = fromIntegral (maxC - minC)
	    h = fromIntegral (maxR - minR)
	dg <- newDrawingGrid w h
	ref <- liftIO $ newIORef cells
	dgSetRenderer dg $ vtvRender ref
	pure (VTV dg ref)

vtvWidget :: MonadIO m => VariationTreeView -> m Widget
vtvWidget = dgWidget . vtvCanvas

vtvSet :: MonadIO m => VariationTreeView -> TreeLayout -> m ()
vtvSet vtv tl = do
	let cells = buildGrid tl
	    (minC, minR, maxC, maxR) = gridBounds cells
	liftIO $ writeIORef (vtvModel vtv) cells
	dgSetSize (vtvCanvas vtv) (fromIntegral (maxC - minC)) (fromIntegral (maxR - minR))
	#queueDraw (vtvCanvas vtv)

vtvRender :: IORef [(GridPos, GridCell)] -> C.Render ()
vtvRender ref = do
	cells <- liftIO $ readIORef ref
	let (minC, minR, _, _) = gridBounds cells
	for_ cells \((col, row), cell) -> do
		C.save
		C.translate (fromIntegral (col - minC)) (fromIntegral (row - minR))
		renderCell cell
		C.restore

renderCell :: GridCell -> C.Render ()
renderCell CellBlank = pure ()
renderCell (CellNode isRoot) = do
	C.setSourceRGB 0 0 0
	C.setLineWidth 0.05
	fitText 0.1 0.1 0.8 0.8 (if isRoot then "ε" else "x")
renderCell (CellEdge k) = do
	C.setSourceRGB 0 0 0
	C.setLineWidth 0.08
	C.setLineCap C.LineCapRound
	C.setLineJoin C.LineJoinRound
	drawEdge k
	C.stroke

-- | Draw edge in 1x1 cell. Midpoints: left (0, 0.5), right (1, 0.5), top (0.5, 1), bottom (0.5, 0).
drawEdge :: EdgeKind -> C.Render ()
drawEdge EdgeStraight = do
	C.moveTo 0 0.5
	C.lineTo 1 0.5
drawEdge EdgeMainToFirst = do
	C.moveTo 0 0.5
	C.lineTo 1 0.5
	C.arcNegative 0.5 0.5 0.5 pi (3*pi/2)
drawEdge EdgeMiddleVariation = do
	C.moveTo 0.5 1
	C.lineTo 0.5 0
	C.arcNegative 0.5 0.5 0.5 (pi/2) 0
drawEdge EdgeConnectingVariation = do
	C.moveTo 0.5 1
	C.lineTo 0.5 0
drawEdge EdgeFinalVariation = do
	C.arcNegative 0.5 0.5 0.5 (pi/2) 0

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
