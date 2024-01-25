{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Fix
import Data.Foldable
import Data.GI.Base.Signals
import Data.IORef
import Data.List
import Data.Maybe
import Data.Traversable
import GHC.Stack
import GI.Gtk
import Nurse.Sveta.Torch.Endpoint
import Nurse.Sveta.Widget
import Nurse.Sveta.Util
import System.Environment
import Util

import qualified Data.Text as T

main :: IO ()
main = do
	torchPlusGtkFix
	app <- new Application []

	on app #activate $ do
		top <- new Box [#orientation := OrientationVertical]
		sel <- new Box [#orientation := OrientationHorizontal, #spacing := 10]
		#append top sel
		ssRef <- newIORef SelectionState
			{ ssTop = exampleEndpoint
			, ssCur = exampleEndpoint
			, ssSelections = []
			, ssXUsed = False
			, ssYUsed = False
			, ssContainer = sel
			, ssChildren = []
			}
		selectorWidget ssRef

		w <- new Window $ tail [undefined
			, #title := "Nurse Sveta endpoint browser"
			, #application := app
			, #child := top
			]
		#show w
	args <- getArgs
	() <$ #run app (Just args)

data SelectionState = SelectionState
	{ ssTop :: Endpoint
	, ssCur :: Endpoint
	, ssSelections :: [Selector]
	, ssXUsed :: Bool
	, ssYUsed :: Bool
	, ssContainer :: Box
	, ssChildren :: [SelectorWidget]
	}

ssForbidden :: SelectionState -> [Axis]
ssForbidden ss = [X | ssXUsed ss] ++ [Y | ssYUsed ss]

ssVectorSelector :: SelectionState -> Maybe (T.Text, [T.Text])
ssVectorSelector ss = vectorSelector (ssSelections ss) (ssCur ss)

ssUnsafeSelect :: HasCallStack => SelectionState -> Selector -> Endpoint
ssUnsafeSelect ss sel = case (ssSelections ss, sel, ssCur ss) of
	([], FVector _, e) -> e
	([], _, e) -> error (show (sel, e))
	(_, FDictionary s, EDictionary dict) -> case lookup s dict of
		Just e -> e
		Nothing -> error $ "tried to select " ++ s ++ " from dictionary with keys " ++ intercalate "," (fst <$> dict) ++ ";\nfull dictionary: " ++ show dict
	(_, FVector _, e) -> case e of
		EFullTensor (d:ds) t | evalGameConstant d > 0 -> EFullTensor ds (t ! 0)
		EMaskedTensor (d:ds) t m | evalGameConstant d > 0 -> EMaskedTensor ds (t ! 0) (m ! 0)
		EVector d (e':_) -> e'
		_ -> error (show e)

unsafeSelect :: HasCallStack => Selector -> Endpoint -> Endpoint
unsafeSelect (FDictionary s) (EDictionary dict) = fromJust (lookup s dict)
unsafeSelect (FVector _) e = case e of
	EFullTensor (_:ds) t -> EFullTensor ds (t ! 0)
	EMaskedTensor (_:ds) t m -> EMaskedTensor ds (t ! 0) (m ! 0)
	EVector _ (e':_) -> e'
	_ -> error (show e)

data SelectorWidget = SelectorWidget
	{ swTop :: Box
	, swList :: ListBox
	, swChoices :: [(ListBoxRow, Selector)]
	}

selectorWidget :: IORef SelectionState -> IO ()
selectorWidget ssRef = do
	ss <- readIORef ssRef
	msw <- case (ssVectorSelector ss, ssCur ss) of
		(Just (header, choices), _) -> Just <$> vectorSelectorWidget (ssForbidden ss) header choices
		(_, EDictionary dict) -> Just <$> dictionarySelectorWidget dict
		_ -> pure Nothing
	for_ msw \sw -> do
		#append (ssContainer ss) (swTop sw)
		writeIORef ssRef ss { ssChildren = ssChildren ss ++ [sw] }
		on (swList sw) #rowSelected \case
			Nothing -> putStrLn "WARNING: got a Nothing in a place I haven't thought about how to handle Nothing yet"
			Just row -> do
				ss' <- readIORef ssRef
				for_ (drop 1 $ zipDrop (ssSelections ss) (ssChildren ss')) \sw' -> do
					#remove (ssContainer ss) (swTop sw')
				let Just sel = lookup row (swChoices sw)
				writeIORef ssRef ss
					{ ssCur = ssUnsafeSelect ss sel
					, ssSelections = ssSelections ss ++ [sel]
					, ssXUsed = ssXUsed ss || sel == FVector X
					, ssYUsed = ssYUsed ss || sel == FVector Y
					, ssChildren = ssChildren ss ++ [sw]
					}
				selectorWidget ssRef

zipDrop :: [a] -> [b] -> [b]
zipDrop (a:as) (b:bs) = zipDrop as bs
zipDrop _ bs = bs

vectorSelector :: [a] -> Endpoint -> Maybe (T.Text, [T.Text])
vectorSelector [] e = Just ("batch size", tshow <$> [0..fromMaybe 0 (batchSize e)-1])
vectorSelector _ e = describeGameConstant <$> endpointGameConstant e

endpointGameConstant :: Endpoint -> Maybe GameConstant
endpointGameConstant = \case
	EVector gc _-> Just gc
	EFullTensor (gc:_) _ -> Just gc
	EMaskedTensor (gc:_) _ _ -> Just gc
	_ -> Nothing

describeGameConstant :: GameConstant -> (T.Text, [T.Text])
describeGameConstant = \case
	GCMiscellaneous n -> (tshow n, tshow <$> [0..n-1])
	GCColors -> ("colors", ["blue", "red", "yellow"])
	GCShapes -> ("shapes", ["x", "⸦", "⸧", "○"])
	GCWidth -> ("width", tshow <$> [0..evalGameConstant GCWidth-1])
	GCHeight -> ("height", tshow <$> [0..evalGameConstant GCHeight-1])
	GCOrientations -> ("orientations", ["horizontal", "vertical"])

emptySelectorWidget :: T.Text -> IO SelectorWidget
emptySelectorWidget header = do
	top <- new Box [#orientation := OrientationHorizontal]
	mid <- new Box [#orientation := OrientationVertical]
	lbl <- new Label [#label := header]
	lst <- new ListBox []
	scr <- new ScrolledWindow [#child := lst, #propagateNaturalWidth := True, #minContentHeight := 300, #vscrollbarPolicy := PolicyTypeExternal]
	adj <- get scr #vadjustment
	bar <- scrollbarNew OrientationVertical (Just adj)
	mfix \callbackId -> on adj #changed do
		up <- get adj #upper
		sz <- get adj #pageSize
		when (sz < up) do
			#append top bar
			disconnectSignalHandler adj callbackId

	#append top mid
	#append mid lbl
	#append mid scr

	pure SelectorWidget { swTop = top, swList = lst, swChoices = [] }

vectorSelectorWidget :: [Axis] -> T.Text -> [T.Text] -> IO SelectorWidget
vectorSelectorWidget forbidden header choices = do
	sw <- emptySelectorWidget header
	choices <- for axes \(axis, description) -> do
		row <- new ListBoxRow [#child :=> new Label [#label := description]]
		#append (swList sw) row
		pure (row, FVector axis)
	pure sw { swChoices = choices }
	where
	axes =  [(axis, describeAxis axis) | axis <- [X,Y,Each,Slider] \\ forbidden]
	     ++ forZipWith [0..] choices \i choice -> (Single i, choice)

dictionarySelectorWidget :: [(String, endpoint)] -> IO SelectorWidget
dictionarySelectorWidget dict = do
	sw <- emptySelectorWidget "dictionary"
	choices <- for dict \(k, _) -> do
		row <- new ListBoxRow [#child :=> new Label [#label := T.pack k]]
		#append (swList sw) row
		pure (row, FDictionary k)
	pure sw { swChoices = choices }

data Axis = X | Y | Each | Slider | Single Int deriving (Eq, Ord, Read, Show)

describeAxis :: Axis -> T.Text
describeAxis = \case
	X -> "x"
	Y -> "y"
	Each -> "each"
	Slider -> "slider"
	Single n -> tshow n

data Selector
	= FDictionary String
	| FVector Axis
	deriving (Eq, Ord, Read, Show)

exampleEndpoint :: Endpoint
exampleEndpoint = EDictionary $ tail [undefined
	, ("A", EFullTensor [GCOrientations, GCWidth, GCHeight] $ generate [3,2,8,16] \[n,o,x,y] -> fromIntegral n/3 + fromIntegral o/6 + fromIntegral x/48 + fromIntegral y/768)
	, ("B", EVector (GCMiscellaneous 1) [EFullTensor [] $ generate [3] \[n] -> fromIntegral n/3])
	]
