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
data GridCell a
	= CellNode (Maybe a) Bool -- ^ the Bool is whether this is the selected node
	| CellEdge [EdgeComponent] (Maybe EdgeComponent) -- ^ edges always shown in [], active path in Maybe
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

-- can't imagine this ever being useful, but the completionist in me has to include it
kittyCorner :: Rendering a -> Rendering a -> Rendering a
kittyCorner ul br = noOverlap <$> combineRenderings False False ul br

rleaf :: a -> Rendering a
rleaf = Rendering 1 1 . flip M.singleton

rempty :: Int -> Int -> Rendering a
rempty w h = Rendering w h def

-- | Grid position (col, row). Even cols = nodes, odd = edges.
type GridPos = (Int, Int)

buildGridFromMoveTree :: MoveSelection -> [Int] -> MoveTree a -> Rendering (GridCell a, MoveTreeAddress)
buildGridFromMoveTree sel active0 t0 = rleaf (CellNode Nothing (sel == def), def) `hcat` case length (mainSequence t0) of
	0 -> goVariations def (Just active0) (variations t0)
	_ -> rleaf (CellEdge [LR] (Just LR), MoveTreeAddress def 0) `hcat` goTree def (Just active0) t0
	where
	goVariations varPath active vs = vcats . toList $ Seq.mapWithIndex (goVariation varPath active (length vs)) vs
	goVariation varPath active n i v
		| i == 0 = (edge [LR, LD] firstVariationHi `vcat` verticalBar) `hcat` child
		| i == n - 1 = edge [UR] laterVariationHi `hcat` child
		| otherwise = (edge [UD, UR] laterVariationHi `vcat` verticalBar) `hcat` child
		where
		varPath' = varPath Seq.:|> i
		active' = case active of
			Just (firstActive : restActive) | firstActive == i -> Just restActive
			_ -> Nothing
		(verticalBarHi, firstVariationHi, laterVariationHi) = case active of
			Just (a:_) -> case compare a i of
				LT -> (Nothing, Nothing, Nothing)
				EQ -> (Nothing, Just LR, Just UR)
				GT -> (Just UD, Just LD, Just UD)
			_ -> (Nothing, Nothing, Nothing)
		child = goTree varPath' active' v
		edge e hi = rleaf (CellEdge e hi, MoveTreeAddress varPath' 0)
		verticalBar = vrep (renderingHeight child - 1) (edge [UD] verticalBarHi)
	goTree varPath active t = mainSeq `hcat` goVariations varPath active (variations t) where
		ns = mainSequence t
		mainSeq = hcats . toList $ Seq.mapWithIndex (goNode varPath active (length ns)) ns
	goNode varPath active n i node
		| i == 0 = cellNode
		| otherwise = cell (CellEdge [LR] (LR <$ active)) `hcat` cellNode
		where
		cell c = rleaf (c, MoveTreeAddress varPath i)
		cellNode = cell (CellNode (Just node) isSelected)
		isSelected = i == mainSequenceIndex sel && isJust active && length varPath == variationDepth sel

cellRowsDefault :: Int
cellRowsDefault = 5

cellSizePx :: Num a => a
cellSizePx = 40

-- the top row is hard to click because you often hit the Paned hitbox instead
-- of the DrawingArea hitbox, so we leave a little space empty at the top
panedOffset :: Double
panedOffset = 0.4

-- | Variation tree view: Cairo-based widget for rendering move trees.
-- Fixed 30-pixel cells, scrollable. Structured for future interactivity.
data VariationTreeView = VTV
	{ vtvCanvas :: DrawingArea
	, vtvAIButton :: CheckButton
	, vtvModel :: IORef (Map GridPos (GridCell (GameStateEdit, GameState), MoveTreeAddress))
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
	pure (VTV da ai ref)

vtvWidget :: MonadIO m => VariationTreeView -> m Widget
vtvWidget = toWidget . vtvCanvas

vtvSet :: MonadIO m => VariationTreeView -> MoveSelection -> [Int] -> MoveTree (GameStateEdit, GameState) -> m ()
vtvSet vtv sel active mt = do
	let grid = buildGridFromMoveTree sel active mt
	    w = renderingWidth grid * cellSizePx
	    h = renderingHeight grid * cellSizePx + ceiling (panedOffset * cellSizePx)
	liftIO $ writeIORef (vtvModel vtv) (renderingTree grid (0, 0))
	#setSizeRequest (vtvCanvas vtv) (fromIntegral w) (fromIntegral h)
	#queueDraw (vtvCanvas vtv)

-- | Install a callback for node clicks. Called with the MoveTreeAddress of the clicked node.
vtvOnNodeClick :: MonadIO m => VariationTreeView -> (MoveTreeAddress -> IO ()) -> m ()
vtvOnNodeClick vtv callback = do
	click <- new GestureClick []
	on click #pressed \_ nX nY -> do
		let col = floor (nX / cellSizePx)
		    row = floor (nY / cellSizePx - panedOffset)
		nodeAddrs <- liftIO $ readIORef (vtvModel vtv)
		for_ (M.lookup (col, row) nodeAddrs) (callback . snd)
	#addController (vtvCanvas vtv) click

vtvRender :: Bool -> Map GridPos (GridCell (GameStateEdit, GameState), MoveTreeAddress) -> C.Render ()
vtvRender aiLol cells = do
	C.setLineCap C.LineCapRound
	C.setLineJoin C.LineJoinRound
	join C.scale cellSizePx
	C.translate 0 panedOffset

	treePath aiLol edgeHighlights
	strokeHighlight

	for_ nodeHighlights \(x_, y_) ->
		let [x, y] = [fromIntegral coord + 0.5 | coord <- [x_, y_]]
		in C.arc x y 0.5 0 (2*pi)
	fillHighlight

	-- we want to make the entire edge path before stroking so that we don't
	-- double-paint on the grid boundaries
	treePath aiLol edges
	strokeTree

	reqss <- for nodes \((x_, y_), mNodeContent) -> do
		let [gridx, gridy] = map fromIntegral [x_, y_]
		case mNodeContent of
			Nothing -> [] <$ fitText (gridx + 0.1) (gridy + 0.9) 0.8 (-0.8) "ε" -- scaled double, so don't participate in TextRequest machinery
			Just (GenerateLevel seed level, _) -> pure $ tail [ignored
				, TextRequest (gridx + 0.1) (gridy + 0.45) 0.8 (-0.35) (printf "%04X" seed)
				, TextRequest (gridx + 0.1) (gridy + 0.9) 0.8 (-0.35) (show level)
				]
			-- TODO: do the fancy location notation thing
			Just (Lock lk mp, _) -> let pc = content (mpPill mp lk) in do
				C.save
				C.scale 0.5 0.5
				let sx = show . (1+) . x . mpBottomLeft $ mp
				    sy = show . (1+) . y . mpBottomLeft $ mp
				reqs <- case orientation pc of
					Horizontal -> lookahead_ (2*gridx) (2*gridy) (lookaheadFromPillContent pc) &> tail [ignored
						, TextRequest (gridx + 0.1) (gridy + 0.9) 0.35 (-0.35) sx
						, TextRequest (gridx + 0.55) (gridy + 0.9) 0.35 (-0.35) sy
						]
					Vertical -> southNorth (2*gridx) (2*gridy) (setColor (otherColor pc)) (setColor (bottomLeftColor pc)) &> tail [ignored
						, TextRequest (gridx + 0.55) (gridy + 0.45) 0.35 (-0.35) sx
						, TextRequest (gridx + 0.55) (gridy + 0.9) 0.35 (-0.35) sy
						]
				reqs <$ C.restore
	fitTexts (concat reqss)
	where
	(nodes, nodeHighlights, edges, edgeHighlights) = M.foldMapWithKey inject cells where
		inject pos (c, _) = case c of
			CellNode medit highlighted -> ([(pos, medit)], [pos | highlighted], [], [])
			CellEdge es ehs -> ([], [], sequence (pos, es), sequence (pos, toList ehs))

strokeHighlight :: Render ()
strokeHighlight = do
	C.setSourceRGBA 0.5 0.75 1 0.4
	for_ [1..4] \i -> do
		C.setLineWidth (lerp (i/5) 0.32 0.08)
		C.strokePreserve
	C.newPath

strokeTree :: Render ()
strokeTree = do
	C.setSourceRGB 0 0 0
	C.setLineWidth 0.08
	C.stroke

fillHighlight :: Render ()
fillHighlight = do
	C.setSourceRGBA 0.5 0.75 1 0.4
	C.fill

treePath :: Bool -> [(GridPos, EdgeComponent)] -> C.Render ()
treePath aiLol = traverse_ \((x_, y_), component) -> do
	let [x, y] = fromIntegral <$> [x_, y_]
	C.translate x y
	edgePath aiLol component
	C.translate (-x) (-y)

-- aiLol: The first version of edgePath was vibe coded. Below is an excerpt
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
edgePath :: Bool -> EdgeComponent -> C.Render ()
edgePath aiLol = \case
	LR -> C.moveTo 0 0.5 >> C.lineTo 1 0.5
	UD -> C.moveTo 0.5 0 >> C.lineTo 0.5 1
	LD -> C.moveTo 0 0.5 >> if aiLol
		then C.arcNegative 0.5 0.5 0.5 pi (pi/2)
		else C.arc 0 1 0.5 (3*pi/2) 0
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
