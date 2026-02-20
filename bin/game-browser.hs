module Main where

import GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Cairo.Render as C
import GI.Gtk hiding (Text)
import Ms.Mendel hiding (get)
import qualified Nurse.Sveta.Cairo as NSC
import Nurse.Sveta.GameBrowser

import qualified Data.Map as M
import qualified Data.Text as T

main :: IO ()
main = do
	torchPlusGtkFix
	app <- new Application []
	on app #activate do
		paned <- new Paned [#orientation := OrientationVertical]
		topRow <- new Box [#orientation := OrientationHorizontal]
		boardView <- newPlayerStateView (uiCurrentPSM def)
		boardWidget <- psvWidget boardView
		boardOverlay <- new Overlay [#child := boardWidget]
		hoverLayer <- new DrawingArea [#hexpand := True, #vexpand := True, #canTarget := False]
		#addOverlay boardOverlay hoverLayer
		treeView <- newVariationTreeView
		treeWidget <- vtvWidget treeView
		treeScroll <- new ScrolledWindow [#child := treeWidget, #hexpand := True]
		tools <- new Box [#orientation := OrientationVertical]
		uiRef <- newIORef (def :: UIModel)
		toolRef <- newIORef initialTool
		dragStartRef <- newIORef (Nothing :: Maybe (Double, Double, Position))
		hoverCellRef <- newIORef (Nothing :: Maybe Position)
		previewPillRef <- newIORef (Nothing :: Maybe Pill)

		seedEntry <- new Entry [#placeholderText := "seed", #maxLength := 4]
		levelEntry <- new Entry [#placeholderText := "level", #maxLength := 2, #inputPurpose := InputPurposeDigits]
		generateButton <- new Button [#label := "generate"]

		toolGroup <- new CheckButton []
		toolButtons <- for allTools \tool -> do
			btn <- new CheckButton [#label := T.pack (pp tool), #group := toolGroup, #active := tool == initialTool]
			btn <$ on btn #toggled do
				active <- get btn #active
				when active (writeIORef toolRef tool)

		let refresh = do
		    	ui <- readIORef uiRef
		    	psvSet boardView (uiCurrentPSM ui)
		    	vtvSet treeView (moveSelection ui) (uiActivePath ui) (nodes ui)
		    	#queueDraw hoverLayer
		vtvOnNodeClick treeView \addr ->
			modifyIORef uiRef (flip uiVisitAddress addr) >> refresh

		drawingAreaSetDrawFunc hoverLayer . Just $ \_ ctx ww wh -> do
			ui <- readIORef uiRef
			mhover <- readIORef hoverCellRef
			mpreview <- readIORef previewPillRef
			drawOverlay ui ww wh ctx mhover mpreview

		set (vtvAIButton treeView) [#label := "ai 👍"]

		motion <- new EventControllerMotion []
		on motion #motion \x y -> do
			cell <- psvPointToBoardCell boardView x y
			writeIORef hoverCellRef cell
			#queueDraw hoverLayer
		on motion #leave do
			writeIORef hoverCellRef Nothing
			#queueDraw hoverLayer
		#addController boardWidget motion

		drag <- new GestureDrag []
		on drag #dragBegin \sx sy -> do
			mcell <- psvPointToBoardCell boardView sx sy
			writeIORef dragStartRef (fmap (\cell -> (sx, sy, cell)) mcell)
			writeIORef previewPillRef Nothing
			#queueDraw hoverLayer
		on drag #dragUpdate \dx dy -> do
			ms <- readIORef dragStartRef
			case ms of
				Nothing -> pure ()
				Just (sx0, sy0, Position sx sy) -> do
					mend <- psvPointToBoardCell boardView (sx0 + dx) (sy0 + dy)
					tool <- readIORef toolRef
					writeIORef previewPillRef (mend >>= \(Position ex ey) -> dragToPill tool (sx, sy) (ex, ey))
					#queueDraw hoverLayer
		on drag #dragEnd \dx dy -> do
			ms <- readIORef dragStartRef
			case ms of
				Nothing -> pure ()
				Just (sx0, sy0, Position sx sy) -> do
					mend <- psvPointToBoardCell boardView (sx0 + dx) (sy0 + dy)
					for_ mend \(Position ex ey) -> do
						tool <- readIORef toolRef
						let mpill = dragToPill tool (sx, sy) (ex, ey)
						for_ mpill \pill -> do
							modifyIORef uiRef \u -> fromMaybe u (uiTryAdvance (Lock pill) u)
							refresh
			writeIORef dragStartRef Nothing
			writeIORef previewPillRef Nothing
			#queueDraw hoverLayer
		#addController boardWidget drag

		seedBuffer <- get seedEntry #buffer
		levelBuffer <- get levelEntry #buffer
		on generateButton #clicked do
			seedMaybe <- parseSeed <$> get seedBuffer #text
			levelMaybe <- parseLevel <$> get levelBuffer #text
			case seedMaybe of
				Nothing -> #addCssClass seedEntry "error"
				Just{} -> #removeCssClass seedEntry "error"
			case levelMaybe of
				Nothing -> #addCssClass levelEntry "error"
				Just{} -> #removeCssClass levelEntry "error"
			for_ seedMaybe \seed -> for_ levelMaybe \level -> do
				modifyIORef uiRef \ui -> fromMaybe ui (uiTryAdvance (GenerateLevel seed level) ui)
				refresh

		#append tools (vtvAIButton treeView)
		mapM_ (#append tools) toolButtons
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

initialTool :: Lookahead
initialTool = head allTools

allTools :: [Lookahead]
allTools = [Lookahead l r | l <- [minBound..maxBound], r <- [l..maxBound]]

dragToPill :: Lookahead -> (Int, Int) -> (Int, Int) -> Maybe Pill
dragToPill (Lookahead c1 c2) (sx, sy) (ex, ey) = case (ex - sx, ey - sy) of
	(1, 0) -> pure (mk Horizontal (sx, sy) c1 c2)
	(-1, 0) -> pure (mk Horizontal (ex, ey) c2 c1)
	(0, 1) -> pure (mk Vertical (sx, sy) c1 c2)
	(0, -1) -> pure (mk Vertical (ex, ey) c2 c1)
	_ -> Nothing
	where
	mk orientation (x, y) bl oc = Pill
		{ content = PillContent
			{ orientation = orientation
			, bottomLeftColor = bl
			, otherColor = oc
			}
		, bottomLeftPosition = Position x y
		}

drawOverlay ui ww wh ctx mhover mpill = flip renderWithContext ctx do
	case mpill of
		Just preview -> do
			let (gx, gy, gw, gh) = gridGeometry
			C.save
			C.translate gx gy
			NSC.initMath (round gw) (round gh) (fromIntegral bw) (fromIntegral bh)
			C.pushGroup
			NSC.pill preview
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
