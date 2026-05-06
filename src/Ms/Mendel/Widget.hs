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

import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified GI.Cairo.Render as C

data GridCell a
	= CellNode (Maybe a) Bool -- ^ the Bool is whether this is the selected node
	| CellEdge [EdgeComponent] (Maybe EdgeComponent) -- ^ edges always shown in [], active path in Maybe
	deriving (Eq, Ord, Read, Show)

instance PP a => PP (GridCell a) where pp = pp1
instance PP1 GridCell where
	liftPP1 ppA = \case
		CellNode ma selected -> maybe "root" ppA ma ++ ['!' | selected]
		CellEdge es mhi -> concatMap pp es ++ maybe "" (('/':) . pp) mhi

data EdgeComponent = LR | UD | LD | UR
	deriving (Eq, Ord, Read, Show)

instance PP EdgeComponent where
	pp = \case
		LR -> "─"
		UD -> "│"
		LD -> "╮"
		UR -> "╰"

data Rendering a = Rendering
	{ renderingWidth, renderingHeight :: Int
	, renderingTree :: Position -> Map Position a
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
	, renderingTree = \posA -> let
		ta = renderingTree as posA
		tb = renderingTree bs Position
			{ x = offset overlapW (x posA) (renderingWidth as)
			, y = offset overlapH (y posA) (renderingHeight as)
			}
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

type UICell = (GridCell UILabel, Variation)

renderModel :: UIModel -> Rendering UICell
renderModel ui = rleaf (CellNode Nothing (selectionDepth ui == 0), []) `hcat` renderForest [] (activeRoot ui) (selectionDepth ui) (nodes ui)

renderForest :: [Int] -> Maybe Int -> Int -> UIForest -> Rendering UICell
renderForest revParentVar active selDepth ts = case IM.toAscList treeRenderings of
	[] -> def
	(i, r) : irs -> go LR LD i r irs
	where
	go toR toD i r = \case
		[] -> edge i [toR] toD toR `hcat` r
		(i', r') : irs -> (verticalBar `hcat` r) `vcat` go UR UD i' r' irs where
			verticalBar = vcat
				(edge i [toR, toD] toD toR)
				(vrep (renderingHeight r - 1) (edge i' [UD] UD UD))

	treeRenderings = IM.mapWithKey (\i -> renderTree (i:revParentVar) (active == Just i) (selDepth - 1)) ts
	edge i always toD toR = rleaf (CellEdge always hi, reverse (i:revParentVar)) where
		hi = case compare (Just i) active of
			LT -> Just toD
			EQ -> Just toR
			GT -> Nothing

renderTree :: [Int] -> Bool -> Int -> UITree -> Rendering UICell
renderTree revVar active selDepth t = hcat
	(rleaf (CellNode (Just (tLabel t)) (active && selDepth == 0), reverse revVar))
	(renderForest revVar (guard active >> activeChild (tLabel t)) selDepth (tChildren t))

renderingForMath :: Rendering a -> Rendering a
renderingForMath r = r { renderingTree = M.mapKeys (\pos -> pos { y = renderingHeight r - y pos - 1 }) . renderingTree r }

cellRowsDefault :: Int
cellRowsDefault = 5

cellSizePx :: Num a => a
cellSizePx = 40

-- the top row is hard to click because you often hit the Paned hitbox instead
-- of the DrawingArea hitbox, so we leave a little space empty at the top
panedOffset :: Double
panedOffset = 0.4

-- | Variation tree view: Cairo-based widget for rendering move trees.
data VariationTreeView = VTV
	{ vtvCanvas :: DrawingGrid
	, vtvAIButton :: CheckButton
	, vtvModel :: IORef (Map Position UICell)
	}

newVariationTreeView :: MonadIO m => m VariationTreeView
newVariationTreeView = do
	dg <- newDrawingGrid 1 (1 + panedOffset)
	ai <- liftIO $ new CheckButton []
	ref <- liftIO $ newIORef def
	let vtv = VTV dg ai ref

	dgSetAlignment dg 0 1
	dgSetDensity dg (Just cellSizePx)
	dgSetRenderer dg (vtvRender vtv)
	
	pure vtv

vtvWidget :: MonadIO m => VariationTreeView -> m Widget
vtvWidget = dgWidget . vtvCanvas

vtvSet :: MonadIO m => VariationTreeView -> UIModel -> m ()
vtvSet vtv ui = do
	liftIO $ writeIORef (vtvModel vtv) (renderingTree r def)
	dgSetSize (vtvCanvas vtv) (fromIntegral (renderingWidth r)) (fromIntegral (renderingHeight r) + panedOffset)
	where
	r = renderingForMath (renderModel ui)

vtvOnNodeClick :: MonadIO m => VariationTreeView -> (Word32 -> Variation -> IO ()) -> m ()
vtvOnNodeClick VTV { vtvCanvas = dg, vtvModel = ref } callback = do
	click <- new GestureClick [#button := 0]
	on click #pressed \_ xPixel yPixel -> do
		(xGrid, yGrid) <- dgPixelToGrid dg (xPixel, yPixel)
		let pos = Position (floor xGrid) (floor yGrid)
		model <- liftIO $ readIORef ref
		button <- #getCurrentButton ?self
		for_ (M.lookup pos model) (callback button . snd)
	w <- dgWidget dg
	#addController w click

vtvRender :: VariationTreeView -> C.Render ()
vtvRender vtv = do
	aiLol <- liftIO $ get (vtvAIButton vtv) #active
	model <- liftIO $ readIORef (vtvModel vtv)
	C.setLineCap C.LineCapRound
	C.setLineJoin C.LineJoinRound

	let (nodes, nodeHighlights, edges, edgeHighlights) = M.foldMapWithKey inject model
	    inject pos (c, _) = case c of
	    	CellNode medit highlighted -> ([(pos, medit)], [pos | highlighted], [], [])
	    	CellEdge es ehs -> ([], [], sequence (pos, es), sequence (pos, toList ehs))
	    act `at` pos = C.save >> translatePosition pos >> act >> C.restore

	treePath aiLol edgeHighlights
	strokeHighlight

	mapM_ (C.arc 0.5 0.5 0.5 0 (2*pi) `at`) nodeHighlights
	fillHighlight

	-- we want to make the entire edge path before stroking so that we don't
	-- double-paint on the grid boundaries
	treePath aiLol edges
	strokeTree

	reqss <- for nodes \(pos, mNodeContent) -> do
		let (gridx, gridy) = fromPosition pos
		case parentEdge <$> mNodeContent of
			Nothing -> [] <$ fitText (gridx + 0.1) (gridy + 0.1) 0.8 0.8 "ε" -- scaled double, so don't participate in TextRequest machinery
			Just (GenerateLevel seed level) -> pure $ tail [ignored
				, TextRequest (gridx + 0.1) (gridy + 0.55) 0.8 0.35 (printf "%04X" seed)
				, TextRequest (gridx + 0.1) (gridy + 0.1) 0.8 0.35 (show level)
				]
			-- TODO: do the fancy location notation thing
			Just (Lock lk mp) -> let pc = content (mpPill mp lk) in do
				C.save
				C.scale 0.5 0.5
				let sx = show . (1+) . x . mpBottomLeft $ mp
				    sy = show . (1+) . y . mpBottomLeft $ mp
				reqs <- case orientation pc of
					Horizontal -> lookahead_ (2*gridx) (2*gridy + 1) (lookaheadFromPillContent pc) &> tail [ignored
						, TextRequest (gridx + 0.1) (gridy + 0.1) 0.35 0.35 sx
						, TextRequest (gridx + 0.55) (gridy + 0.1) 0.35 0.35 sy
						]
					Vertical -> southNorth (2*gridx) (2*gridy) (setColor (otherColor pc)) (setColor (bottomLeftColor pc)) &> tail [ignored
						, TextRequest (gridx + 0.55) (gridy + 0.55) 0.35 0.35 sx
						, TextRequest (gridx + 0.55) (gridy + 0.1) 0.35 0.35 sy
						]
				reqs <$ C.restore
	fitTexts (concat reqss)

setHighlightSource, strokeHighlight, strokeTree, fillHighlight :: Render ()
setHighlightSource = C.setSourceRGBA 0.5 0.75 1 0.4
strokeHighlight = setHighlightSource >> C.setLineWidth 0.3 >> C.stroke
strokeTree = C.setSourceRGB 0 0 0 >> C.setLineWidth 0.08 >> C.stroke
fillHighlight = setHighlightSource >> C.fill

treePath :: Bool -> [(Position, EdgeComponent)] -> C.Render ()
treePath aiLol = traverse_ \(pos, component) -> do
	C.save
	translatePosition pos
	edgePath aiLol component
	C.restore

fromPosition :: Position -> (Double, Double)
fromPosition pos = (fromIntegral (x pos), fromIntegral (y pos))

translatePosition :: Position -> C.Render ()
translatePosition = uncurry C.translate . fromPosition

-- aiLol: The first version of edgePath was written by an AI. Below is an
-- excerpt from the prompt I wrote describing how I wanted things drawn. The AI
-- I was using sort of did what I said... but definitely didn't do what I
-- meant. I mean I would bet it basically understood what I meant and just got
-- the arguments to the arc functions a bit wrong, and I don't blame it, at the
-- time there were like eight coordinate transforms going on and I would
-- struggle to get it right the first time, too. But it does look just
-- hilarious, and I decided I liked it enough to keep it around.
--
-- * No variations: line left to right
-- * Connecting the end of a main sequence to the first variation: line left to right and arc left to bottom
-- * Middle variations: line top to bottom and arc top to right
-- * Connecting variations: line top to bottom
-- * Final variation: arc top to right
edgePath :: Bool -> EdgeComponent -> C.Render ()
edgePath aiLol = \case
	LR -> C.moveTo 0 0.5 >> C.lineTo 1 0.5
	UD -> C.moveTo 0.5 0 >> C.lineTo 0.5 1
	LD -> C.moveTo 0.5 0 >> if aiLol
		then C.arcNegative 0.5 0.5 0.5 (-pi/2) pi
		else C.arc 0 0 0.5 0 (pi/2)
	UR -> C.moveTo 0.5 1 >> if aiLol
		then C.arc 0.5 0.5 0.5 (pi/2) 0
		else C.arc 1 1 0.5 pi (3*pi/2)

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
