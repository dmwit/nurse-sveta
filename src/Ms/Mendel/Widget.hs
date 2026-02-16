module Ms.Mendel.Widget
	( module Ms.Mendel.Widget
	, module Nurse.Sveta.Widget
	) where

import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Cairo.Render as C
import GI.Gtk
import Ms.Mendel.Cairo
import Ms.Mendel.Population
import Nurse.Sveta.Cairo (fitText)
import Nurse.Sveta.GameBrowser (MoveTree(..))
import Nurse.Sveta.Util
import Nurse.Sveta.Widget

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

-- | Build the grid from a tree layout. Returns (cell map, minCol, minRow, colCount, rowCount).
-- Row 0 = main line. Variations stacked in rows 1+.
buildGrid :: TreeLayout -> (Map.Map GridPos GridCell, Int, Int, Int, Int)
buildGrid tl = (cells, minCol, minRow, colCount, rowCount)
	where
	assocList = go 0 0 tl
	cells = Map.fromList assocList
	positions = map fst assocList
	(minCol, minRow, maxCol, maxRow) = case positions of
		[] -> (0, 0, 0, 0)
		_ -> (minimum (map fst positions), minimum (map snd positions)
		     , maximum (map fst positions), maximum (map snd positions))
	colCount = max 1 (maxCol - minCol + 1)
	rowCount = max 1 (maxRow - minRow + 1)
	go col row (TreeLayout n vars) = mainNodes ++ mainEdges ++ variationCells
		where
		mainNodes = [ ((col + 2*i, row), CellNode (i == 0)) | i <- [0..n] ]
		mainEdges = case vars of
			[] -> [ ((col + 2*i + 1, row), CellEdge EdgeStraight) | i <- [0..n-1] ]
			_ -> let
				straightEdges = [ ((col + 2*i + 1, row), CellEdge EdgeStraight) | i <- [0..n-2] ]
				junctionEdgeCol = col + max 1 (2*n - 1)
				junctionEdge = ((junctionEdgeCol, row), CellEdge EdgeMainToFirst)
				in straightEdges ++ [junctionEdge]
		junctionCol = col + 2 * max 0 (n - 1)
		stemCol = col + max 1 (2*n - 1)
		variationCells = case vars of
			[] -> []
			_ -> let
				rowStarts = scanl (+) 1 (map variationRows vars)
				stemRows = [1 .. last rowStarts - 1]
				stemCells = [ ((stemCol, r), kindFor r) | r <- stemRows ]
				kindFor r
					| r == 1 = EdgeMiddleVariation
					| r == last rowStarts - 1 = EdgeFinalVariation
					| otherwise = EdgeConnectingVariation
				stemCellList = map (\(pos, k) -> (pos, CellEdge k)) stemCells
				varCells = concatMap (\(r0, v) -> go junctionCol r0 v) $
					zip rowStarts vars
				in stemCellList ++ varCells

cellSizePx :: Int
cellSizePx = 30

-- | Variation tree view: Cairo-based widget for rendering move trees.
-- Fixed 30-pixel cells, scrollable. Structured for future interactivity.
data VariationTreeView = VTV
	{ vtvCanvas :: DrawingArea
	, vtvModel :: IORef (Map.Map GridPos GridCell, Int, Int, Int, Int)
	-- ^ (cells, minCol, minRow, colCount, rowCount)
	}

newVariationTreeView :: MonadIO m => TreeLayout -> m VariationTreeView
newVariationTreeView tl = do
	let (cells, minC, minR, colCount, rowCount) = buildGrid tl
	    w = colCount * cellSizePx
	    h = rowCount * cellSizePx
	da <- new DrawingArea []
	ref <- liftIO $ newIORef (cells, minC, minR, colCount, rowCount)
	drawingAreaSetDrawFunc da . Just $ \_ ctx _ _ ->
		flip renderWithContext ctx =<< (vtvRender <$> liftIO (readIORef ref))
	#setSizeRequest da (fromIntegral w :: Int32) (fromIntegral h :: Int32)
	pure (VTV da ref)

vtvWidget :: MonadIO m => VariationTreeView -> m Widget
vtvWidget = toWidget . vtvCanvas

vtvSet :: MonadIO m => VariationTreeView -> TreeLayout -> m ()
vtvSet vtv tl = do
	let (cells, minC, minR, colCount, rowCount) = buildGrid tl
	liftIO $ writeIORef (vtvModel vtv) (cells, minC, minR, colCount, rowCount)
	#setSizeRequest (vtvCanvas vtv) (fromIntegral (colCount * cellSizePx) :: Int32) (fromIntegral (rowCount * cellSizePx) :: Int32)
	#queueDraw (vtvCanvas vtv)

vtvRender :: (Map.Map GridPos GridCell, Int, Int, Int, Int) -> C.Render ()
vtvRender (cells, minC, minR, _colCount, _rowCount) = do
	for_ (Map.toList cells) \((col, row), cell) -> do
		C.save
		C.translate (fromIntegral (col - minC) * fromIntegral cellSizePx) (fromIntegral (row - minR) * fromIntegral cellSizePx)
		C.scale (fromIntegral cellSizePx) (fromIntegral cellSizePx)
		renderCell cell
		C.restore

renderCell :: GridCell -> C.Render ()
renderCell CellBlank = pure ()
renderCell (CellNode isRoot) = do
	C.setSourceRGB 0 0 0
	C.setLineWidth 0.05
	-- fitText expects math coords (y up); flip cell to match
	C.translate 0 1
	C.scale 1 (-1)
	fitText 0.1 0.1 0.8 0.8 (if isRoot then "ε" else "x")
renderCell (CellEdge k) = do
	C.setSourceRGB 0 0 0
	C.setLineWidth 0.08
	C.setLineCap C.LineCapRound
	C.setLineJoin C.LineJoinRound
	drawEdge k
	C.stroke

-- | Draw edge in 1x1 cell. Screen coords (y down): left (0, 0.5), right (1, 0.5), top (0.5, 0), bottom (0.5, 1).
drawEdge :: EdgeKind -> C.Render ()
drawEdge EdgeStraight = do
	C.moveTo 0 0.5
	C.lineTo 1 0.5
drawEdge EdgeMainToFirst = do
	C.moveTo 0 0.5
	C.lineTo 1 0.5
	C.arcNegative 0.5 0.5 0.5 pi (pi/2)  -- left to bottom (clockwise)
drawEdge EdgeMiddleVariation = do
	C.moveTo 0.5 0
	C.lineTo 0.5 1
	C.arcNegative 0.5 0.5 0.5 (3*pi/2) 0  -- top to right (clockwise)
drawEdge EdgeConnectingVariation = do
	C.moveTo 0.5 0
	C.lineTo 0.5 1
drawEdge EdgeFinalVariation = do
	C.arcNegative 0.5 0.5 0.5 (3*pi/2) 0  -- top to right

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
