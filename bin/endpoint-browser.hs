{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Fix
import Data.Default
import Data.Foldable
import Data.GI.Base.Signals
import Data.IORef
import Data.List
import Data.Maybe
import Data.Traversable
import GHC.Stack
import GI.Gtk
import Nurse.Sveta.Torch.Endpoint
import Nurse.Sveta.Cairo
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
		sli <- new Box [#orientation := OrientationVertical]
		grf <- newHomogeneousGridView
		#append top sel
		#append top sli
		#append top =<< hgvWidget grf

		ssRef <- newIORef SelectionState
			{ ssTop = exampleEndpoint
			, ssCur = newResidue exampleEndpoint
			, ssSelections = newPath
			, ssContainer = sel
			, ssChildren = []
			}
		gsRef <- newIORef GraphsState
			{ gsSliders = []
			, gsSliderContainer = sli
			, gsGraphContainer = grf
			, gsPreviousDrawArguments = Nothing
			}
		selectorWidget gsRef ssRef

		w <- new Window $ tail [undefined
			, #title := "Nurse Sveta endpoint browser"
			, #application := app
			, #child := top
			]
		#show w
	args <- getArgs
	() <$ #run app (Just args)

data GraphsState = GraphsState
	{ gsSliders :: [Scale]
	, gsSliderContainer :: Box
	, gsGraphContainer :: HomogeneousGridView
	, gsPreviousDrawArguments :: Maybe (Path, [Int])
	}

resetGraphs :: IORef GraphsState -> Endpoint -> Path -> IO ()
resetGraphs gsRef e path = do
	gs <- readIORef gsRef
	removeAll (gsSliderContainer gs)
	scales <- sliders (newResidue e) (getPath path)
	writeIORef gsRef gs { gsSliders = scales }
	for_ scales \scale -> do
		#append (gsSliderContainer gs) scale
		on scale #valueChanged do
			drawGraphs gsRef e path . map round =<< traverse #getValue scales
	drawGraphs gsRef e path (0 <$ scales)

drawGraphs :: IORef GraphsState -> Endpoint -> Path -> [Int] -> IO ()
drawGraphs gsRef e path is = do
	gs <- readIORef gsRef
	when (currentDrawArguments /= gsPreviousDrawArguments gs) do
		graphs <- for hms \hm -> do
			dg <- newDrawingGrid (hsFullWidth stats) (hsFullHeight stats)
			dgSetRenderer dg if hsBoard stats
				then boardHeatmapWith (hsOptions stats) def (PillContent Horizontal Blue Yellow) (hHeat hm)
				else labeledHeatmapWith (hsOptions stats) (hsMapWidth stats) (hsMapHeight stats) (hHeat hm)
			dgWidget dg
		hgvSetModel (gsGraphContainer gs) HGM
			{ hgmIndividualWidth = hsFullWidth stats
			, hgmIndividualHeight = hsFullHeight stats
			, hgmChildren = graphs
			}
		writeIORef gsRef gs { gsPreviousDrawArguments = currentDrawArguments }
	where
	hms = heatmaps e path is
	stats = summarizeHeatmaps hms
	currentDrawArguments = Just (path, is)

data HeatmapsSummary = HeatmapsSummary
	{ hsBoard :: Bool
	, hsMapWidth :: Int
	, hsMapHeight :: Int
	, hsFullWidth :: Double
	, hsFullHeight :: Double
	, hsOptions :: HeatmapOptions
	}

summarizeHeatmaps :: [Heatmap] -> HeatmapsSummary
summarizeHeatmaps hms@(hm:_) = HeatmapsSummary
	{ hsBoard = isBoard
	, hsMapWidth = mw
	, hsMapHeight = mh
	, hsFullWidth = fromIntegral fw
	, hsFullHeight = fromIntegral fh
	, hsOptions = opts
	} where
	isBoard = hWidth hm == GCWidth && hHeight hm == GCHeight
	allFloats = [v | hm <- hms, row <- hContents hm, Just v <- row]
	lo = case allFloats of [] -> 0; _ -> minimum allFloats
	hi = case allFloats of [] -> 1; _ -> maximum allFloats
	mw = evalGameConstant (hWidth hm)
	mh = evalGameConstant (hHeight hm)
	(fw, fh) = if isBoard
		then boardHeatmapSizeRecommendation def
		else labeledHeatmapSizeRecommendation mw mh
	opts
		| 0 <= lo && hi <= 1 && (hi-lo) >= 0.1 = heatmapOptions01
		| 0 == lo = heatmapOptions0Max hi
		| 0 < lo && hi/lo >= 3 = heatmapOptions0Max hi
		| otherwise = heatmapOptionsRange lo hi
summarizeHeatmaps [] = HeatmapsSummary
	{ hsBoard = False
	, hsMapWidth = 1
	, hsMapHeight = 1
	, hsFullWidth = 1
	, hsFullHeight = 1
	, hsOptions = heatmapOptions01
	}

sliders :: HasCallStack => Residue -> [Selector] -> IO [Scale]
sliders r (FVector Slider:path) = case residueBranch r of
	BVector _ ts -> do
		scale <- scaleNewWithRange OrientationHorizontal 0 (fromIntegral (length ts-1)) 1
		forZipWithM_ [0..] ts \i t -> #addMark scale i PositionTypeTop (Just t)
		(scale:) <$> sliders (r `extendResidue` FVector Slider) path
	BDictionary{} -> error "tried to make a slider for a dictionary"
	Leaf -> error "tried to make a slider for a leaf"
sliders r (sel:path) = sliders (r `extendResidue` sel) path
sliders r [] = pure []

data SelectionState = SelectionState
	{ ssTop :: Endpoint
	, ssCur :: Residue
	, ssSelections :: Path
	, ssContainer :: Box
	, ssChildren :: [SelectorWidget]
	}

data SelectorWidget = SelectorWidget
	{ swTop :: Box
	, swList :: ListBox
	, swChoices :: [(ListBoxRow, Selector)]
	}

selectorWidget :: IORef GraphsState -> IORef SelectionState -> IO ()
selectorWidget gsRef ssRef = do
	ss <- readIORef ssRef
	msw <- case residueBranch (ssCur ss) of
		BVector nm vals -> Just <$> vectorSelectorWidget (forbidden (ssCur ss)) nm vals
		BDictionary ks -> Just <$> dictionarySelectorWidget ks
		Leaf -> Nothing <$ resetGraphs gsRef (ssTop ss) (ssSelections ss)
	for_ msw \sw -> do
		#append (ssContainer ss) (swTop sw)
		writeIORef ssRef ss { ssChildren = ssChildren ss ++ [sw] }
		on (swList sw) #rowSelected \case
			Nothing -> putStrLn "WARNING: got a Nothing in a place I haven't thought about how to handle Nothing yet"
			Just row -> do
				ss' <- readIORef ssRef
				for_ (drop 1 $ zipDrop (getBackwardsPath (ssSelections ss)) (ssChildren ss')) \sw' -> do
					#remove (ssContainer ss) (swTop sw')
				let Just sel = lookup row (swChoices sw)
				writeIORef ssRef ss
					{ ssCur = ssCur ss `extendResidue` sel
					, ssSelections = ssSelections ss `extendPath` sel
					, ssChildren = ssChildren ss ++ [sw]
					}
				selectorWidget gsRef ssRef

zipDrop :: [a] -> [b] -> [b]
zipDrop (a:as) (b:bs) = zipDrop as bs
zipDrop _ bs = bs

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
	nonsensical = if length choices < 2 then [Each, Slider] else []
	axes =  [(axis, describeAxis axis) | axis <- [X,Y,Each,Slider] \\ (forbidden ++ nonsensical)]
	     ++ forZipWith [0..] choices \i choice -> (Single i, choice)

dictionarySelectorWidget :: [T.Text] -> IO SelectorWidget
dictionarySelectorWidget ks = do
	sw <- emptySelectorWidget "dictionary"
	choices <- for ks \k -> do
		row <- new ListBoxRow [#child :=> new Label [#label := k]]
		#append (swList sw) row
		pure (row, FDictionary (T.unpack k))
	pure sw { swChoices = choices }

data Axis = X | Y | Each | Slider | Single Int deriving (Eq, Ord, Read, Show)

describeAxis :: Axis -> T.Text
describeAxis = \case
	X -> "x"
	Y -> "y"
	Each -> "each"
	Slider -> "slider"
	Single n -> tshow n

axisIndexRepresentative :: Axis -> Int
axisIndexRepresentative = \case
	Single i -> i
	_ -> 0

data Selector
	= FDictionary String
	| FVector Axis
	deriving (Eq, Ord, Read, Show)

newtype Path = Path [Selector] deriving (Eq, Ord, Read, Show)

newPath :: Path
newPath = Path []

extendPath :: Path -> Selector -> Path
extendPath (Path ss) s = Path (s:ss)

getPath :: Path -> [Selector]
getPath (Path ss) = reverse ss

-- | faster, but selectors are in reverse order
getBackwardsPath :: Path -> [Selector]
getBackwardsPath (Path ss) = ss

data Branch = Leaf | BVector T.Text [T.Text] | BDictionary [T.Text] deriving (Eq, Ord, Read, Show)

bVector :: T.Text -> [T.Text] -> Branch
bVector nm = \case
	[] -> Leaf
	vals -> BVector nm vals

data BatchSelection = NotYetSelected | Pending Int | Selected deriving (Eq, Ord, Read, Show)
data Residue = Residue
	-- invariant: if the representative is a tensor, the batchSelection is Selected
	{ representative :: Endpoint
	, batchSelection :: BatchSelection
	, xUsed :: Bool
	, yUsed :: Bool
	} deriving (Eq, Ord, Read, Show)

newResidue :: Endpoint -> Residue
newResidue e = Residue
	{ representative = e
	, batchSelection = NotYetSelected
	, xUsed = False
	, yUsed = False
	}

outermostVectorSize :: HasCallStack => Residue -> GameConstant
outermostVectorSize r = case batchSelection r of
	NotYetSelected -> GCMiscellaneous (batchSize' (representative r))
	_ -> case outermostGameConstant (representative r) of
		Just gc -> gc
		Nothing -> error $ "Attempted to request the vector size of a dictionary residue\nResidue: " ++ show r

normalizeBatchSelection :: Residue -> Residue
normalizeBatchSelection r = case (batchSelection r, representative r) of
	(Pending b, EFullTensor gcs v) -> r { batchSelection = Selected, representative = EFullTensor gcs (v ! b) }
	(Pending b, EMaskedTensor gcs v m) -> r { batchSelection = Selected, representative = EMaskedTensor gcs (v ! b) (m ! b) }
	_ -> r

useAxis :: Axis -> Residue -> Residue
useAxis axis r = normalizeBatchSelection r
	{ xUsed = xUsed r || axis == X
	, yUsed = yUsed r || axis == Y
	}

extendResidue :: HasCallStack => Residue -> Selector -> Residue
extendResidue r@(Residue { batchSelection = NotYetSelected }) = \case
	FVector axis -> useAxis axis r { batchSelection = Pending (axisIndexRepresentative axis) }
	FDictionary k -> error $ "Attempted to extend a residue through dictionary key " ++ k ++ ", but no batch index has been selected yet\nResidue: " ++ show r
extendResidue r@(Residue { representative = e }) = \case
	FVector axis -> useAxis axis case e of
		-- we maintain an invariant that tensor representatives have already
		-- had their batch index selected, so the first axis of the
		-- StridedVector is really the first GameConstant axis, too
		EFullTensor (gc:gcs) v | evalGameConstant gc > i -> r { representative = EFullTensor gcs (v ! i) }
		EMaskedTensor (gc:gcs) v m | evalGameConstant gc > i -> r { representative = EMaskedTensor gcs (v ! i) (m ! i) }
		EVector gc es | evalGameConstant gc > i -> r { representative = es !! i }
		EDictionary _ -> error $ "Attempted to extend a residue containing a dictionary representative with a vector selector\nAxis: " ++ show axis ++ "\nResidue: " ++ show r
		_ -> error $ "Attempted to extend a residue with fewer than " ++ show (i+1) ++ " children in its representative\nAxis: " ++ show axis ++ "\nResidue: " ++ show r
		where
		i = axisIndexRepresentative axis
	FDictionary k -> case e of
		EDictionary es -> case lookup k es of
			Just e' -> normalizeBatchSelection r { representative = e' }
			Nothing -> error $ "Attempted to extend a residue through dictionary key " ++ k ++ ", but that key isn't currently available\nResidue: " ++ show r
		_ -> error $ "Attempted to extend a non-dictionary residue through dictionary key " ++ k ++ "\nResidue: " ++ show r

forbidden :: Residue -> [Axis]
forbidden r = [X | xUsed r] ++ [Y | yUsed r]

residueBranch :: Residue -> Branch
residueBranch r@(Residue { batchSelection = NotYetSelected })
	= bVector "batch size" (tshow <$> [0..batchSize' (representative r)-1])
residueBranch r = case representative r of
	EFullTensor (gc:_) _ -> gameConstantBranch gc
	EMaskedTensor (gc:_) _ _ -> gameConstantBranch gc
	EVector gc _ -> gameConstantBranch gc
	EDictionary es -> BDictionary (T.pack . fst <$> es)
	_ -> Leaf

gameConstantBranch :: GameConstant -> Branch
gameConstantBranch gc = bVector (gameConstantName gc) (gameConstantInhabitants gc)

gameConstantName :: GameConstant -> T.Text
gameConstantName = \case
	GCMiscellaneous n -> tshow n
	GCColors -> "colors"
	GCShapes -> "shapes"
	GCWidth -> "width"
	GCHeight -> "height"
	GCOrientations -> "orientations"

gameConstantInhabitants :: GameConstant -> [T.Text]
gameConstantInhabitants = \case
	GCColors -> ["blue", "red", "yellow"]
	GCShapes -> ["x", "⸦", "⸧", "○"]
	GCOrientations -> ["horizontal", "vertical"]
	gc -> tshow <$> [0..evalGameConstant gc-1]

data Heatmap = Heatmap
	{ hWidth :: GameConstant
	, hHeight :: GameConstant
	, hContents :: [[Maybe Float]]
	} deriving (Eq, Ord, Read, Show)

heatmaps :: HasCallStack => Endpoint -> Path -> [Int] -> [Heatmap]
heatmaps e p is0 = go (newResidue e) is0 (getPath p) where
	go r is = \case
		FVector axis : sels -> combine axis gc (go' <$> bs) where
			gc = outermostVectorSize r
			go' b = go (r `extendResidue` FVector (Single b)) is' sels
			(bs, is') = case axis of
				Single i -> ([i], is)
				Slider -> case is of
					[] -> error "path says to use the next slider index, but we've already used all the sliders available"
					i:is' -> ([i], is')
				_ -> ([0..evalGameConstant gc-1], is)
		FDictionary k : sels -> go (r `extendResidue` FDictionary k) is sels
		[] -> [Heatmap
			{ hWidth = GCMiscellaneous 1
			, hHeight = GCMiscellaneous 1
			, hContents = [[case representative r of
				EFullTensor _ vals -> justTheFloat vals
				EMaskedTensor _ vals mask -> if the mask == 0 then Nothing else justTheFloat vals
				_ -> error $ "Reached the end of the path, but still looking at a dictionary, not a particular number\nResidue: " ++ show r ++ "\nOriginal path: " ++ show p
				]]
			}]
	justTheFloat = Just . realToFrac . the
	combine axis sz heatmapTable = case axis of
		X -> [ Heatmap
		     	{ hWidth = sz
		     	, hHeight = hHeight (head hms)
		     	, hContents = concat . transpose $ map hContents hms
		     	}
		     | hms <- transpose heatmapTable
		     ]
		Y -> [ Heatmap
		     	{ hWidth = hWidth (head hms)
		     	, hHeight = sz
		     	, hContents = map concat . transpose $ map hContents hms
		     	}
		     | hms <- transpose heatmapTable
		     ]
		Each -> concat (transpose heatmapTable)
		Slider -> concat heatmapTable
		Single _ -> concat heatmapTable

hHeat :: Heatmap -> [(Position, Float)]
hHeat hm =
	[ (Position x y, val)
	| (x, col) <- zip [0..] (hContents hm)
	, (y, Just val) <- zip [0..] col
	]

exampleEndpoint :: Endpoint
exampleEndpoint = EDictionary $ tail [undefined
	, ("A", EFullTensor [GCOrientations, GCWidth, GCHeight] $ generate [3,2,8,16] \[n,o,x,y] -> fromIntegral n/3 + fromIntegral o/6 + fromIntegral x/48 + fromIntegral y/768)
	, ("B", EVector (GCMiscellaneous 1) [EFullTensor [] $ generate [3] \[n] -> fromIntegral n/3])
	]
