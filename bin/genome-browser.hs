module Main where

import Control.Monad
import Data.Aeson
import Data.Foldable
import Data.IORef
import Data.Traversable
import Data.Vector (Vector)
import GI.Gtk
import Nurse.Sveta.Cairo
import Nurse.Sveta.Files
import Nurse.Sveta.Genome
import Nurse.Sveta.Widget
import System.Environment
import System.Mem
import Util

import qualified Data.Vector as V
import qualified GI.Cairo.Render as C
import qualified Nurse.Sveta.Cairo as NC

-- ╭╴w╶──────╮
-- │╭╴top╶──╮│
-- ││╭╴gen╶╮││
-- ││╰─────╯││
-- ││╭╴ivd╶╮││
-- ││╰─────╯││
-- ││╭╴hgv╶╮││
-- ││╰─────╯││
-- │╰───────╯│
-- ╰─────────╯
main :: IO ()
main = do
	torchPlusGtkFix
	dir <- getXdgDirectory XdgData "ms-mendel"
	genI0 <- eitherDecodeFileStrict (dir </> "latest.json") >>= either fail pure
	pop0 <- loadPopulation dir genI0
	popRef <- newIORef pop0
	let genF0 = fromInteger genI0
	app <- new Application []
	on app #activate do
		top <- new Box [#orientation := OrientationVertical, #spacing := 4]
		gen <- new SpinButton [#adjustment :=> new Adjustment [#upper := genF0, #stepIncrement := 1], #value := genF0]
		ivd <- new SpinButton [#adjustment :=> new Adjustment [#upper := fromIntegral (V.length pop0 - 1), #stepIncrement := 1]]
		hgv <- newHomogeneousGridView

		let refreshGrid = do
		    	ivdI <- round <$> #getValue ivd
		    	pop <- readIORef popRef
		    	let ivdG = pop V.! ivdI
		    	    ivdW = fromIntegral (gConvWidth ivdG)
		    	    ivdH = fromIntegral (gConvHeight ivdG)
		    	    w = max 3 ivdW
		    	    h = ivdH + 1
		    	children <- for [0..gSize ivdG-1] \i -> patternWidget ivdG i ivdW ivdH w h
		    	hgvSetModel hgv HGM
		    		{ hgmIndividualWidth = w
		    		, hgmIndividualHeight = h
		    		, hgmChildren = children
		    		}

		#append top gen
		#append top ivd
		#append top =<< hgvWidget hgv
		refreshGrid

		on gen #valueChanged do
			genI <- round <$> #getValue gen
			pop <- loadPopulation dir genI
			writeIORef popRef pop
			adj <- #getAdjustment ivd
			ivdF <- #getValue ivd
			let bound = fromIntegral (V.length pop - 1)
			set adj [#upper := bound, #value := min ivdF bound]
			refreshGrid

		on ivd #valueChanged refreshGrid

		w <- new Window $ tail [undefined
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

loadPopulation :: FilePath -> Integer -> IO (Vector Genome)
loadPopulation dir gen = eitherDecodeFileStrict (dir </> show gen <.> "json") >>= \case
	Left err -> fail err
	Right (RecordOfVectors pop) -> traverse gFromSpec pop

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
