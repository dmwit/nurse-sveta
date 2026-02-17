module Main where

import Nurse.Sveta.Util
import Nurse.Sveta.Widget
import GI.Gtk as G

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

main :: IO ()
main = do
	torchPlusGtkFix
	app <- new Application []
	on app #activate $ do
		psv <- newPlayerStateView (PSM board Nothing [])
		psvWidget psv >>= \w -> set w [#heightRequest := 500]
		places <- new ListBox [#activateOnSingleClick := True]
		for_ (sort . HM.toList . unsafeApproxReachable board $ launchPill (Lookahead Blue Red)) $ \(pill, move) -> do
			btn <- new Button $ tail [ignored
				, #label := T.pack (pp pill)
				, On #clicked (setOverlay psv move)
				]
			#append places btn
		sw <- new ScrolledWindow [#child := places, #heightRequest := 500]
		box <- new Box [#orientation := OrientationVertical, #widthRequest := 1000]
		psvWidget psv >>= #append box
		#append box sw
		w <- new Window $ tail [ignored
			, #title := "Nurse Sveta pathfinding demo"
			, #application := app
			, #child := box
			]
		#show w
	args <- getArgs
	() <$ #run app (Just args)

board :: Board
board = randomBoard 48 6

setOverlay :: PlayerStateView -> BoxMove -> IO ()
setOverlay psv move = psvModify_ psv $ \psm -> psm { psmOverlay = overlay } where
	overlay = [(pill1, 0.3), (pill2, 0.3), (pill3, 0.3), (pill4, 1)]
	pill0 = launchPill (Lookahead Blue Red)
	pill1 = fromMaybe pill0 (rotate board pill0 (initialRotation move))
	pill2 = pill1 { bottomLeftPosition = pos1 }
	pill3 = pill2 { bottomLeftPosition = pos2 }
	pill4 = fromMaybe pill3 (finalRotation move >>= rotate board pill3)
	pos0 = bottomLeftPosition pill0
	pos1 = pos0 { x = x pos0 + xDelta move }
	pos2 = pos1 { y = y pos1 + yDelta move }
