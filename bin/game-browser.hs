module Main where

import GI.Gtk hiding (Text)
import Ms.Mendel hiding (get)
import Nurse.Sveta.GameBrowser

import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T

main :: IO ()
main = do
	torchPlusGtkFix
	app <- new Application []
	on app #activate do
		top <- new Box [#orientation := OrientationHorizontal]
		boardView <- newPlayerStateView (uiCurrentPSM def)
		boardWidget <- psvWidget boardView
		treeView <- new ListBox [#activateOnSingleClick := True, #selectionMode := SelectionModeNone]
		treeScroll <- new ScrolledWindow [#child := treeView, #minContentWidth := 550]
		tools <- new Box [#orientation := OrientationVertical]
		uiRef <- newIORef (def :: UIModel)
		toolRef <- newIORef (Blue, Blue)
		dragStartRef <- newIORef (Nothing :: Maybe (Double, Double, Position))

		seedEntry <- new Entry [#placeholderText := "seed", #maxLength := 4]
		levelEntry <- new Entry [#placeholderText := "level", #maxLength := 2, #inputPurpose := InputPurposeDigits]
		generateButton <- new Button [#label := "generate"]

		toolButtons <- for lookaheadTools \(c1, c2) -> do
			btn <- new ToggleButton [#label := ppTool c1 c2]
			pure (btn, c1, c2)
		for_ toolButtons \(btn, c1, c2) -> on btn #toggled do
				active <- get btn #active
				when active do
					writeIORef toolRef (c1, c2)
					for_ toolButtons \(btn', c1', c2') -> when ((c1, c2) /= (c1', c2')) do
						set btn' [#active := False]
		case toolButtons of
			((btn, _, _):_) -> set btn [#active := True]
			[] -> pure ()

		let refresh = do
		    	ui <- readIORef uiRef
		    	psvSet boardView (uiCurrentPSM ui)
		    	renderTreeView treeView boardView uiRef

		drag <- new GestureDrag []
		on drag #dragBegin \sx sy -> do
			mcell <- psvPointToBoardCell boardView sx sy
			writeIORef dragStartRef (fmap (\cell -> (sx, sy, cell)) mcell)
		on drag #dragEnd \dx dy -> do
			ms <- readIORef dragStartRef
			case ms of
				Nothing -> pure ()
				Just (sx0, sy0, Position sx sy) -> do
					mend <- psvPointToBoardCell boardView (sx0 + dx) (sy0 + dy)
					for_ mend \(Position ex ey) -> do
						(c1, c2) <- readIORef toolRef
						let mpill = dragToPill c1 c2 (sx, sy) (ex, ey)
						for_ mpill \pill -> do
							modifyIORef uiRef \u -> fromMaybe u (uiTryAdvance (Lock pill) u)
							refresh
			writeIORef dragStartRef Nothing
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

		for_ toolButtons \(btn, _c1, _c2) -> #append tools btn
		#append tools seedEntry
		#append tools levelEntry
		#append tools generateButton

		#append top boardWidget
		#append top treeScroll
		#append top tools

		w <- new Window $ tail [ignored
			, #title := "Ms. Mendel Game Browser"
			, #application := app
			, #child := top
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

lookaheadTools :: [(Color, Color)]
lookaheadTools =
	[ (Blue, Blue)
	, (Blue, Red)
	, (Blue, Yellow)
	, (Red, Red)
	, (Red, Yellow)
	, (Yellow, Yellow)
	]

ppTool :: Color -> Color -> Text
ppTool c1 c2 = T.pack (take 1 (show c1) ++ take 1 (show c2))

dragToPill :: Color -> Color -> (Int, Int) -> (Int, Int) -> Maybe Pill
dragToPill c1 c2 (sx, sy) (ex, ey) = case (ex - sx, ey - sy) of
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

data TreeRow = TreeRow
	{ trAddress :: MoveTreeAddress
	, trLabel :: Text
	} deriving (Eq, Ord, Read, Show)

renderTreeView :: ListBox -> PlayerStateView -> IORef UIModel -> IO ()
renderTreeView treeView boardView uiRef = do
	listBoxRemoveAll treeView
	ui <- readIORef uiRef
	for_ (uiTreeRows ui) \tr -> do
		btn <- new Button [#label := trLabel tr, #halign := AlignStart]
		when (trAddress tr == selectedAddress ui) do
			#setSensitive btn False
		on btn #clicked do
			modifyIORef uiRef \ui' -> fromMaybe ui' (uiVisitAddress ui' (trAddress tr))
			ui'' <- readIORef uiRef
			psvSet boardView (uiCurrentPSM ui'')
			renderTreeView treeView boardView uiRef
		#append treeView btn
	where
	selectedAddress ui = MoveTreeAddress (uiActivePath ui) (uiMainSequenceIndex ui)

uiTreeRows :: UIModel -> [TreeRow]
uiTreeRows ui = go Seq.empty (nodes ui) where
	go path mt = moveRows ++ branchRows where
		moveRows = flip map (zip [0..] (toList (mainSequence mt))) \case
			(i, (edit, _gs)) -> TreeRow
				{ trAddress = MoveTreeAddress path i
				, trLabel = rowLabel (length path) i edit
				}
		branchRows = flip concatMap (zip [0..] (toList (variations mt))) \case
			(i, child) -> go (path Seq.|> i) child

	rowLabel depth i edit = ""
		<> T.replicate depth "  "
		<> T.pack (printf "[%02d] " i)
		<> ppEdit edit

	ppEdit = \case
		GenerateLevel seed level -> T.pack (printf "generate %04x level %d" seed level)
		Lock pill -> "lock " <> T.pack (show pill)
