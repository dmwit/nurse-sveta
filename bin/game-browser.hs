module Main where

import GI.Gtk hiding (Text)
import Ms.Mendel hiding (get)
import Nurse.Sveta.GameBrowser

import qualified Data.Map as M
import qualified Data.Text as T

main :: IO ()
main = do
	torchPlusGtkFix
	app <- new Application []
	on app #activate do
		top <- new Box [#orientation := OrientationHorizontal]
		boardView <- newPlayerStateView (uiCurrentPSM def)
		tools <- new Box [#orientation := OrientationVertical]
		uiRef <- newIORef (def :: UIModel)

		seedEntry <- new Entry [#placeholderText := "seed", #maxLength := 4]
		levelEntry <- new Entry [#placeholderText := "level", #maxLength := 2, #inputPurpose := InputPurposeDigits]
		generateButton <- new Button [#label := "generate"]

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
				readIORef uiRef >>= psvSet boardView . uiCurrentPSM

		#append tools seedEntry
		#append tools levelEntry
		#append tools generateButton

		#append top =<< psvWidget boardView
		#append top tools

		w <- new Window $ tail [ignored
			, #title := "Ms. Mendel Game Browser"
			, #application := app
			, #child := top
			]
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
