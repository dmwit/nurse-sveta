module Main where

import GI.Cairo.Render.Connector (renderWithContext)
import GI.Gdk.Flags
import GI.Gtk hiding (Text)
import Ms.Mendel hiding (get)
import Nurse.Sveta.GameBrowser

import qualified GI.Cairo.Render as C
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Nurse.Sveta.Cairo as NC

main :: IO ()
main = do
	torchPlusGtkFix
	app <- new Application []
	on app #activate do
		paned <- new Paned [#orientation := OrientationVertical]
		topRow <- new Box [#orientation := OrientationHorizontal]
		boardView <- newPlayerStateView (uiCurrentPSM def)
		boardWidget <- psvWidget boardView
		boardOverlay <- new Overlay [#child := boardWidget, #heightRequest := 500, #widthRequest := 250]
		hoverLayer <- new DrawingArea [#hexpand := True, #vexpand := True, #canTarget := False]
		#addOverlay boardOverlay hoverLayer
		treeView <- newVariationTreeView
		treeWidget <- vtvWidget treeView
		treeScroll <- new ScrolledWindow [#child := treeWidget, #hexpand := True, #heightRequest := fromIntegral (cellSizePx * cellRowsDefault)]
		scrollAxisSwap <- new EventControllerScroll [#flags := [EventControllerScrollFlagsBothAxes]]
		tools <- new Box [#orientation := OrientationVertical]
		uiRef <- newIORef (def :: UIModel)
		toolRef <- newIORef initialTool
		hoverCellRef <- newIORef (Nothing :: Maybe Position)
		previewPillRef <- newIORef (Nothing :: Maybe Pill)

		seedEntry <- new Entry [#placeholderText := "seed", #maxLength := 4]
		levelEntry <- new Entry [#placeholderText := "level", #maxLength := 2, #inputPurpose := InputPurposeDigits]
		generateButton <- new Button [#label := "generate"]

		toolGroup <- new CheckButton []
		toolButtons <- for allTools \tool -> do
			tw <- toolWidget tool (get (vtvAIButton treeView) #active)
			btn <- new CheckButton [#child := twTop tw, #group := toolGroup, #active := tool == initialTool]
			btn <$ on btn #toggled do
				active <- get btn #active
				when active (writeIORef toolRef tool)
			toWidget btn <&> \top' -> tw { twTop = top' }

		let refresh = do
		    	ui <- readIORef uiRef
		    	psvSet boardView (uiCurrentPSM ui)
		    	vtvSet treeView (moveSelection ui) (uiActivePath ui) (nodes ui)
		    	forM_ toolButtons \tw -> twRefresh tw ui
		    	#queueDraw hoverLayer
		vtvOnNodeClick treeView \addr ->
			modifyIORef uiRef (flip uiVisitAddress addr) >> refresh
		on scrollAxisSwap #scroll \dx dy -> do
			-- You would think that dx contains the horizontal scroll distance,
			-- and dy contains the vertical scroll distance. But no, dx
			-- contains 0 and dy contains the scroll distance regardless of
			-- direction.
			mevent <- #getCurrentEvent scrollAxisSwap
			case mevent of
				Just ev -> do
					modifiers <- #getModifierState ev
					adjustment <- if ModifierTypeShiftMask `elem` modifiers
						then get treeScroll #vadjustment
						else get treeScroll #hadjustment
					increment <- get adjustment #stepIncrement
					value <- get adjustment #value
					True <$ set adjustment [#value := value + dy * increment]
				Nothing -> pure False
		#addController treeScroll . fromJust =<< castTo EventController =<< #ref scrollAxisSwap

		drawingAreaSetDrawFunc hoverLayer . Just $ \_ ctx ww wh -> do
			ui <- readIORef uiRef
			mhover <- readIORef hoverCellRef
			mpreview <- readIORef previewPillRef
			drawOverlay ui ww wh ctx mhover mpreview

		set (vtvAIButton treeView) [#label := "ai 👍"]
		on (vtvAIButton treeView) #toggled refresh

		motion <- new EventControllerMotion []
		on motion #motion \x y -> do
			cell <- psvPointToBoardCell boardView x y
			writeIORef hoverCellRef cell
			#queueDraw hoverLayer
		on motion #leave do
			writeIORef hoverCellRef Nothing
			#queueDraw hoverLayer
		#addController boardWidget motion

		let dragToRotatedPill drag dx dy = #getStartPoint drag >>= \case
		    	(True, x, y) -> do
		    		#queueDraw hoverLayer
		    		start <- psvPointToBoardCell boardView x y
		    		end <- psvPointToBoardCell boardView (x + dx) (y + dy)
		    		for (liftJ2 dragToMidPlacement start end) \mp -> do
		    			lk <- liftA2 toolLookahead (readIORef toolRef) (readIORef uiRef)
		    			pure (RotatedPill lk mp)
		    	_ -> fail "trying to inspect a drag that isn't currently happening"
		drag <- new GestureDrag []
		on drag #dragUpdate \dx dy -> do
			mrp <- dragToRotatedPill ?self dx dy
			writeIORef previewPillRef (rpPill <$> mrp)
		on drag #dragEnd \dx dy -> do
			mrp <- dragToRotatedPill ?self dx dy
			for_ mrp \rp -> do
				modifyIORef uiRef (uiMaybeAdvance (rpGameStateEdit rp))
				refresh
			writeIORef previewPillRef Nothing
		#addController boardWidget drag

		seedBuffer <- get seedEntry #buffer
		levelBuffer <- get levelEntry #buffer
		let generateLevel = do
			    seedMaybe <- parseSeed <$> get seedBuffer #text
			    levelMaybe <- parseLevel <$> get levelBuffer #text
			    case seedMaybe of
			    	Nothing -> #addCssClass seedEntry "error"
			    	Just{} -> #removeCssClass seedEntry "error"
			    case levelMaybe of
			    	Nothing -> #addCssClass levelEntry "error"
			    	Just{} -> #removeCssClass levelEntry "error"
			    for_ seedMaybe \seed -> for_ levelMaybe \level -> do
			    	modifyIORef uiRef (uiMaybeAdvance (GenerateLevel seed level))
			    	let seed' = runST do
			    	    	mrng <- mnewRNG seed
			    	    	munsafeRandomLevel mrng level
			    	    	mrng
			    	set seedBuffer [#text := T.pack (printf "%04X" seed')]
			    	set levelBuffer [#text := tshow (min 24 (level + 1))]
			    	refresh
		on generateButton #clicked generateLevel
		on seedEntry #activate generateLevel
		on levelEntry #activate generateLevel

		#append tools (vtvAIButton treeView)
		mapM_ (#append tools . twTop) toolButtons
		#append tools seedEntry
		#append tools levelEntry
		#append tools generateButton

		#append topRow boardOverlay
		#append topRow tools
		set paned [#startChild := topRow, #endChild := treeScroll]

		w <- new Window $ tail [ignored
			, #title := "Ms. Mendel Game Browser"
			, #application := app
			, #child := paned
			]
		refresh
		#show w

	args <- getArgs
	() <$ #run app (Just args)

parseSeed :: Text -> Maybe Word16
parseSeed = \t -> do
	ws <- mapM (flip M.lookup digits) (T.unpack t)
	guard (length ws <= 4)
	ensure (>1) $ foldl' (\n w -> shiftL n 4 .|. w) 0 ws
	where
	digits = M.fromList $ []
		++ zip ['0'..'9'] [0..]
		++ zip ['a'..'f'] [10..]
		++ zip ['A'..'F'] [10..]

parseLevel :: Text -> Maybe Int
parseLevel t = tread t >>= ensure (\n -> 0 <= n && n <= 20)

data RotatedPill = RotatedPill
	{ rpLookahead :: Lookahead
	, rpPlacement :: MidPlacement
	} deriving (Eq, Ord, Read, Show)

rpPill :: RotatedPill -> Pill
rpPill rp = mpPill (rpPlacement rp) (rpLookahead rp)

rpGameStateEdit :: RotatedPill -> GameStateEdit
rpGameStateEdit rp = Lock (rpLookahead rp) (rpPlacement rp)

data Tool
	= Exact Lookahead
	| SeedLookahead
	| ActiveLookahead
	deriving (Eq, Ord, Read, Show)

initialTool :: Tool
initialTool = head allTools

allTools :: [Tool]
allTools = ActiveLookahead : SeedLookahead : map Exact [Lookahead l r | l <- [minBound..maxBound], r <- [l..maxBound]]

toolDensity :: Num a => a
toolDensity = 20

data ToolWidget = ToolWidget
	{ twTop :: Widget
	, twRefresh :: UIModel -> IO ()
	}

toolWidget :: Tool -> IO Bool -> IO ToolWidget
toolWidget tool aiLolAct = do
	container <- new Box [#orientation := OrientationHorizontal]
	dg <- newDrawingGrid 2 1
	dgSetDensity dg (Just toolDensity)
	#append container =<< dgWidget dg
	top <- toWidget container

	ToolWidget top <$> case tool of
		Exact{} -> pure (dgSetRenderer dg . renderTool tool)
		SeedLookahead -> do
			seedSvg <- getDataFileName "img/seed.svg"
			wseed <- imageNewFromFile seedSvg
			set wseed [#pixelSize := toolDensity]
			#append container wseed
			pure \ui -> dgSetRenderer dg (renderTool tool ui) >> #queueDraw dg
		ActiveLookahead -> do
			dg' <- newDrawingGrid 1 1
			dgSetDensity dg' (Just toolDensity)
			dgSetRenderer dg' do
				-- sigh
				C.translate 0 1
				C.scale 1 (-1)

				aiLol <- liftIO aiLolAct
				C.translate 0.1 0.1
				C.scale 0.8 0.8
				edgePath aiLol UR
				strokeHighlight
				edgePath aiLol UD
				edgePath aiLol UR
				strokeTree
			#append container =<< dgWidget dg'
			pure \ui -> dgSetRenderer dg (renderTool tool ui) >> #queueDraw dg >> #queueDraw dg'

toolLookahead :: Tool -> UIModel -> Lookahead
toolLookahead tool ui = fromMaybe (Lookahead Blue Blue) case tool of
	Exact lk -> Just lk
	SeedLookahead -> uiSeedLookahead ui
	ActiveLookahead -> uiActiveLookahead ui <|> uiSeedLookahead ui

toolIntensity :: Tool -> Float
toolIntensity = \case
	Exact{} -> 1
	_ -> 0.2

renderTool :: Tool -> UIModel -> C.Render ()
renderTool tool ui = NC.westEast 0 0
	(C.setSourceRGB (mix lr) (mix lg) (mix lb))
	(C.setSourceRGB (mix rr) (mix rg) (mix rb))
	where
	lk = toolLookahead tool ui
	(lr, lg, lb) = NC.cairoColor (leftColor lk)
	(rr, rg, rb) = NC.cairoColor (rightColor lk)
	mix = lerp (toolIntensity tool) 1

dragToMidPlacement :: Position -> Position -> Maybe MidPlacement
dragToMidPlacement start end = case (x end - x start, y end - y start) of
	( 1,  0) -> pure (MidPlacement start 0)
	( 0, -1) -> pure (MidPlacement end 1)
	(-1,  0) -> pure (MidPlacement end 2)
	( 0,  1) -> pure (MidPlacement start 3)
	_ -> Nothing

drawOverlay ui ww wh ctx mhover mpill = flip renderWithContext ctx do
	case mpill of
		Just preview -> do
			let (gx, gy, gw, gh) = gridGeometry
			C.save
			C.translate gx gy
			NC.initMath (round gw) (round gh) (fromIntegral bw) (fromIntegral bh)
			C.pushGroup
			NC.pill preview
			C.popGroupToSource
			C.paintWithAlpha 0.6
			C.restore
		Nothing -> for_ mhover \pos -> do
			for_ (cellRect pos) \(x, y, cw, ch) -> do
				C.setLineWidth 2
				C.setSourceRGBA 1 1 1 0.9
				C.rectangle x y cw ch
				C.strokePreserve
				C.setSourceRGBA 1 1 1 (0.9 * 0.15)
				C.fill
	where
	cellRect (Position cx cy) =
		let (gx, gy, gw, gh) = gridGeometry
		    cw = gw / fromIntegral bw
		    ch = gh / fromIntegral bh
		    x0 = gx + cw * fromIntegral (cx + 1)
		    y0 = gy + gh * (1 - fromIntegral (cy + 2) / fromIntegral bh)
		in [(x0, y0, cw, ch)]

	gridGeometry =
		let ww' = fromIntegral ww
		    wh' = fromIntegral wh
		    aspect = fromIntegral bw / fromIntegral bh
		in if ww' / wh' > aspect
			then
				let drawW = wh' * aspect
				    offX = (ww' - drawW) / 2
				in (offX, 0, drawW, wh')
			else
				let drawH = ww' / aspect
				    offY = (wh' - drawH) / 2
				in (0, offY, ww', drawH)

	b = board (uiCurrentState ui)
	bw = width b + 2
	bh = height b + 4
