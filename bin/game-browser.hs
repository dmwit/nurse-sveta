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
		treeView <- new ListBox [#activateOnSingleClick := True, #selectionMode := SelectionModeNone]
		treeScroll <- new ScrolledWindow [#child := treeView, #minContentWidth := 550]
		tools <- new Box [#orientation := OrientationVertical]
		uiRef <- newIORef (def :: UIModel)

		seedEntry <- new Entry [#placeholderText := "seed", #maxLength := 4]
		levelEntry <- new Entry [#placeholderText := "level", #maxLength := 2, #inputPurpose := InputPurposeDigits]
		generateButton <- new Button [#label := "generate"]
		let refresh = do
		    	ui <- readIORef uiRef
		    	psvSet boardView (uiCurrentPSM ui)
		    	renderTreeView treeView boardView uiRef

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

		#append tools seedEntry
		#append tools levelEntry
		#append tools generateButton

		#append top =<< psvWidget boardView
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
