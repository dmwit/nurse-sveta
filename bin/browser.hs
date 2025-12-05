module Main where

import GI.Gtk
import Ms.Mendel hiding (get)
import Nurse.Sveta.Cairo
import Nurse.Sveta.Widget

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified GI.Cairo.Render as C
import qualified Nurse.Sveta.Cairo as NC

-- ╭╴w╶──────╮
-- │╭╴top╶──╮│
-- ││╭╴gen╶╮││
-- ││╰─────╯││
-- ││╭╴ivd╶╮││
-- ││╰─────╯││
-- ││╭╴cnv╶╮││
-- ││╰─────╯││
-- ││╭╴hgv╶╮││
-- ││╰─────╯││
-- │╰───────╯│
-- ╰─────────╯
main :: IO ()
main = do
	torchPlusGtkFix
	dir <- basedir XdgData
	(genI0, pop0) <- loadPopulation_ dir
	popRef <- newIORef pop0
	let genF0 = fromIntegral genI0
	app <- new Application []
	on app #activate do
		top <- new Box [#orientation := OrientationVertical, #spacing := 4]
		gen <- new SpinButton [#adjustment :=> new Adjustment [#upper := genF0, #stepIncrement := 1], #value := genF0]
		ivd <- new SpinButton [#adjustment :=> new Adjustment [#upper := fromIntegral (V.length pop0 - 1), #stepIncrement := 1]]
		cnv <- dropDownNewFromStrings . iSizesText . V.head $ pop0
		hgv <- newHomogeneousGridView

		let refreshGrid = do
		    	ivdI <- round <$> #getValue ivd
		    	pop <- readIORef popRef
		    	cnvI <- fromIntegral <$> get cnv #selected
		    	let ivdV = pop V.! ivdI
		    	    cnvV = iSizes ivdV !! cnvI
		    	    cnvG = iGenes ivdV HM.! cnvV
		    	    cnvW = fromIntegral (csWidth cnvV)
		    	    cnvH = fromIntegral (csHeight cnvV)
		    	    w = cnvW
		    	    h = cnvH + 1
		    	children <- for [0..gSize cnvG-1] \i -> patternWidget cnvG i cnvW cnvH w h
		    	hgvSetModel hgv HGM
		    		{ hgmIndividualWidth = w
		    		, hgmIndividualHeight = h
		    		, hgmChildren = children
		    		}

		#append top gen
		#append top ivd
		#append top cnv
		#append top =<< hgvWidget hgv
		refreshGrid

		on gen #valueChanged do
			genI <- round <$> #getValue gen
			pop <- loadGeneration_ dir genI
			writeIORef popRef pop
			adj <- #getAdjustment ivd
			ivdF <- #getValue ivd
			let bound = fromIntegral (V.length pop - 1)
			set adj [#upper := bound, #value := min ivdF bound]
			refreshGrid

		on ivd #valueChanged do
			ivdI <- round <$> #getValue ivd
			pop <- readIORef popRef
			updateStrings cnv (iSizesText (pop V.! ivdI))
			refreshGrid

		on cnv (PropertyNotify #selected) \_ -> refreshGrid

		w <- new Window $ tail [ignored
			, #title := "Ms. Mendel Genome Browser"
			, #application := app
			, #child := top
			, #defaultWidth := 1500
			, #defaultHeight := 1000
			, On #closeRequest (performGC >> #quit app >> pure True)
			]

		#show w
	args <- getArgs
	() <$ #run app (Just args)

iSizes :: Individual -> [ConvolutionSize]
iSizes = sort . HM.keys . iGenes

iSizesText :: Individual -> [T.Text]
iSizesText = map (fromString . csPretty) . iSizes

updateStrings :: DropDown -> [T.Text] -> IO ()
updateStrings cnv lbls0 = do
	Just model_ <- get cnv #model
	Just model <- castTo StringList model_
	n <- get model #nItems
	-- TODO: test this lmao
	let loopForward i []
	    	| i == n = pure ()
	    	| otherwise = #splice model i (n-i) Nothing
	    loopForward i (lbl:lbls)
	    	| i == n = #splice model i 0 (Just (lbl:lbls))
	    	| otherwise = do
	    		Just lbl' <- #getString model i
	    		if lbl == lbl'
	    			then loopForward (i+1) lbls
	    			else loopBackward i n (reverse (lbl:lbls))
	    loopBackward i j [] = #splice model i (j-i) Nothing
	    loopBackward i j (lbl:lbls)
	    	| i == j = #splice model i 0 (Just (reverse (lbl:lbls)))
	    	| otherwise = do
	    		Just lbl' <- #getString model j
	    		if lbl == lbl'
	    			then loopBackward i (j-1) lbls
	    			else #splice model i (j-i) (Just (reverse (lbl:lbls)))
	loopForward 0 lbls0

-- genome, pattern index, convolution width, convolution height, widget width, widget height
patternWidget :: Genome -> Int -> Double -> Double -> Double -> Double -> IO Widget
patternWidget g i cw ch ww wh = do
	dg <- newDrawingGrid ww wh
	dgSetRenderer dg do
		for_ [0..cw-1] \x ->
			for_ [0..ch-1] \y -> do
				C.save
				C.translate x (y+1)
				let xI = round x; yI = round y
				renderPatternCell (\c -> gGetColorPattern g i c xI yI) (\s -> gGetShapePattern g i s xI yI)
				C.restore
		fitText 0 0 ww 1 . show $ gGetPatternScore g i
	dgWidget dg

renderPatternCell :: (WithSentinels Color -> Bool) -> (WithSentinels Shape -> Bool) -> Render ()
renderPatternCell getColor getShape = do
	C.save
	when everythingIsAllowed do
		C.setSourceRGBA 0 1 0 0.2
		C.rectangle 0 0 1 1
		C.fill
	when nothingIsAllowed do
		C.setSourceRGBA 1 0 0 0.2
		C.rectangle 0 0 1 1
		C.fill
	C.scale 0.2 0.2
	for_ [(Blue, 0), (Red, 1), (Yellow, 2)] \(c, x) -> disallowed
		(getColor (NonSentinel c))
		(NC.shape x 2 (setColor c) Disconnected)
	for_ [(Virus, -0.5), (Disconnected, 0.5), (West, 1.5), (East, 2.5)] \(s, x) -> disallowed
		(getShape (NonSentinel s))
		(NC.shape x 1 neutral s)
	disallowed (getSentinel EmptySentinel) (fitText 1.5 1 1 1 "ε")
	disallowed (getSentinel OutOfBoundsSentinel) (fitText 2.5 1 1 1 "x")
	C.restore
	where
	getSentinel :: (forall a. WithSentinels a) -> Bool
	getSentinel sentinel = getColor sentinel || getShape sentinel

	nsColors = NonSentinel <$> [Blue, Red, Yellow]
	nsShapes = NonSentinel <$> [Virus, Disconnected, West, East]

	everythingIsAllowed = not $ any getColor (EmptySentinel:OutOfBoundsSentinel:nsColors)
	                         || any getShape (EmptySentinel:OutOfBoundsSentinel:nsShapes)

	nothingIsAllowed = and [True
		, getSentinel EmptySentinel
		, getSentinel OutOfBoundsSentinel
		, all getColor nsColors || all getShape nsShapes
		]

	disallowed d act = do
		C.pushGroup
		act
		C.popGroupToSource
		C.paintWithAlpha if d then 0.1 else 1
