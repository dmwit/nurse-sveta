module Nurse.Sveta.Cairo (
	initMath, bottleSizeRecommendation,
	bottleWithLookahead, bottle, bottleMaybeLookahead,
	bottleOutline, bottleContent, lookahead, lookaheadContent,
	pill, cell, setColor, neutral,
	fitText, fitTexts, TextRequest(..),
	-- * Heatmaps
	--
	-- | Heatmaps display a board with a colored background to convey numerical
	-- information associated with positions on the board. When you supply
	-- numerical information, all the numbers should be at unique board
	-- positions that are in-bounds for the board. (This is not checked.)
	-- Heatmaps also come with a legend describing the connection between
	-- colors and numbers.
	heatmapSizeRecommendation,
	heatmap01, heatmap0Dyn, heatmap0Max, heatmapDyn, heatmapRange, heatmapWith,
	HeatmapOptions(..), heatmapOptions01, heatmapOptions0Max, heatmapOptionsDyn, heatmapOptionsDyn', heatmapOptionsRange,
	bwGradient, saturatingGradient, bSaturatingGradient,
	) where

import Control.Applicative
import Control.Monad
import Data.Fixed
import Data.Foldable
import Data.Monoid
import GI.Cairo.Render hiding (RectangleInt(..))
import GI.Cairo.Render.Matrix (Matrix(..))
import Dr.Mario.Model
import GHC.Stack
import Numeric
import Nurse.Sveta.Util

-- | Under normal circumstances, cairo coordinates are (0,0) at the top left,
-- with x increasing to the right and y increasing down. This converts to (0,0)
-- at the bottom left, with x increasing to the right and y increasing up.
--
-- A call to @initMath w0 h0 w1 h1@ takes converts a coordinate system in which
-- @(w0, h0)@ is just off-screen to the bottom right to a coordinate system in
-- which @(w1, h1)@ is just off-screen to the top right. If the aspect ratios
-- do not match, the image will appear stretched, so maybe try to avoid that.
initMath :: Integral a => a -> a -> Double -> Double -> Render ()
initMath w0 h0 w1 h1 = do
	scale (fromIntegral w0 / w1) (-fromIntegral h0 / h1)
	translate 0 (-h1)

-- | If you want to render a board, you should reserve a rectangle with these
-- dimensions to render it into.
bottleSizeRecommendation :: Board -> (Int, Int)
bottleSizeRecommendation b = (width b + 2, height b + 4)

bottle :: Board -> Render ()
bottle b = bottleOutline (width b) (height b) >> bottleContent b

bottleWithLookahead :: Board -> (Color, Color) -> Render ()
bottleWithLookahead b (l, r) = do
	bottleOutline_ w h xMid
	bottleContent b
	lookahead_ w h xMid l r
	where
	w = fromIntegral (width b)
	h = fromIntegral (height b)
	xMid = xMidFromWidth (width b)

bottleMaybeLookahead :: Board -> Maybe (Color, Color) -> Render ()
bottleMaybeLookahead = liftA2 maybe bottle bottleWithLookahead

-- | width, height
bottleOutline :: Int -> Int -> Render ()
bottleOutline w h = bottleOutline_ (fromIntegral w) (fromIntegral h) (xMidFromWidth w)

bottleOutline_ :: Double -> Double -> Double -> Render ()
bottleOutline_ w h xMid = do
	setSourceRGB 0 0 0
	setLineWidth 0.4
	setLineCap LineCapRound
	setLineJoin LineJoinRound
	centered moveTo (head coordinates)
	mapM_ (centered lineTo) (tail coordinates)
	stroke
	where
	coordinates = tail [undefined
		, (xMid-1, h+3)
		, (xMid-1, h+2)
		, (xMid, h+2)
		, (xMid, h+1)
		, (0, h+1)
		, (0, 0)
		, (w+1, 0)
		, (w+1, h+1)
		, (xMid+3, h+1)
		, (xMid+3, h+2)
		, (xMid+4, h+2)
		, (xMid+4, h+3)
		]

bottleContent :: Board -> Render ()
bottleContent = getAp . ofoldMapWithKey
	(\(Position x y) -> Ap . cell (fromIntegral x) (fromIntegral y))

-- | width, height, left, right
lookahead :: Int -> Int -> Color -> Color -> Render ()
lookahead w h = lookahead_ (fromIntegral w) (fromIntegral h) (xMidFromWidth w)

lookahead_ :: Double -> Double -> Double -> Color -> Color -> Render ()
lookahead_ w h xMid l r = lookaheadContent_ w h xMid (PillContent Horizontal l r)

-- | width, height, content
lookaheadContent :: Int -> Int -> PillContent -> Render ()
lookaheadContent w h = lookaheadContent_ (fromIntegral w) (fromIntegral h) (xMidFromWidth w)

lookaheadContent_ :: Double -> Double -> Double -> PillContent -> Render ()
lookaheadContent_ w h xMid pc = case orientation pc of
	Horizontal -> do
		cell  xMid      (h+1.5) (Occupied (bottomLeftColor pc) West )
		cell (xMid+1  ) (h+1.5) (Occupied (     otherColor pc) East )
	Vertical -> do
		cell (xMid+0.5) (h+1  ) (Occupied (bottomLeftColor pc) South)
		cell (xMid+0.5) (h+2  ) (Occupied (     otherColor pc) North)

pill :: Pill -> Render ()
pill Pill
	{ content = PillContent
		{ orientation = dir
		, bottomLeftColor = bl
		, otherColor = o
		}
	, bottomLeftPosition = Position (fromIntegral -> x) (fromIntegral -> y)
	} = case dir of
		Horizontal -> cell  x     y    (Occupied bl West )
		           >> cell (x+1)  y    (Occupied o  East )
		Vertical   -> cell  x     y    (Occupied bl South)
		           >> cell  x    (y+1) (Occupied o  North)

cell :: Double -> Double -> Cell -> Render ()
cell _ _ Empty = pure ()
cell x_ y_ (Occupied c s) = do
	case s of
		Virus -> do
			centered moveTo pos
			centered arc pos 0.4 0 (2*pi)
			closePath
		Disconnected -> do
			centered moveTo pos
			centered arc pos 0.3 0 (2*pi)
			closePath
		North -> do
			moveTo (x+0.8) (y+0.5)
			centered arc pos 0.3 0 pi
			lineTo (x+0.2) (y+0.1)
			lineTo (x+0.8) (y+0.1)
			closePath
		South -> do
			moveTo (x+0.2) (y+0.5)
			centered arc pos 0.3 pi (2*pi)
			lineTo (x+0.8) (y+0.9)
			lineTo (x+0.2) (y+0.9)
			closePath
		West -> do
			moveTo (x+0.5) (y+0.8)
			centered arc pos 0.3 (pi/2) (3*pi/2)
			lineTo (x+0.9) (y+0.2)
			lineTo (x+0.9) (y+0.8)
			closePath
		East -> do
			moveTo (x+0.5) (y+0.2)
			centered arc pos 0.3 (3*pi/2) (5*pi/2)
			lineTo (x+0.1) (y+0.8)
			lineTo (x+0.1) (y+0.2)
			closePath

	setSourceRGB 0 0 0
	setLineWidth 0.05
	strokePreserve
	setColor c
	fill

	when (s == Virus) $ do
		setSourceRGB 0 0 0
		moveTo (x+0.35) (y+0.65)
		arc (x+0.35) (y+0.65) 0.1 0 (2*pi)
		moveTo (x+0.65) (y+0.65)
		arc (x+0.65) (y+0.65) 0.1 0 (2*pi)
		fill
		setLineWidth 0.1
		moveTo (x+0.35) (y+0.35)
		lineTo (x+0.65) (y+0.35)
		stroke

	where
	x = x_ + 1
	y = y_ + 1
	pos = (x, y)

setColor :: Color -> Render ()
setColor = \case
	Blue -> setSourceRGB 0.13 0.49 0.72
	Red -> setSourceRGB 0.99 0.39 0.41
	Yellow -> setSourceRGB 0.82 0.79 0.26

-- | Convert an operation that normally operates on grid boundaries to operate
-- on grid centers instead.
centered :: (Double -> Double -> a) -> (Double, Double) -> a
centered f (x,y) = f (x+0.5) (y+0.5)

xMidFromWidth :: Int -> Double
xMidFromWidth w = fromIntegral $ (w-1) `quot` 2

gradientStops :: [(Double, Double, Double)]
gradientStops = tail [undefined
	, (0.27, 0.00, 0.00)
	, (0.78, 0.78, 0.00)
	, (0.00, 0.82, 0.20)
	, (0.78, 0.78, 0.98)
	]

bwGradient :: HasCallStack => Double -> Render ()
bwGradient n
	| n <= 0 = setSourceRGB 0 0 0
	| n >= 1 = setSourceRGB 1 1 1
	| otherwise = unsafeGradient n

saturatingGradient :: HasCallStack => Double -> Render ()
saturatingGradient = unsafeGradient . max 0 . min 1

bSaturatingGradient :: HasCallStack => Double -> Render ()
bSaturatingGradient n
	| n <= 0 = setSourceRGB 0 0 0
	| otherwise = unsafeGradient $ min 1 n

unsafeGradient :: HasCallStack => Double -> Render ()
unsafeGradient n = setSourceRGB (m*r + m'*r') (m*g + m'*g') (m*b + m'*b') where
	segments = fromIntegral (length gradientStops) - 1
	(d, m_) = n `divMod'` recip segments
	m' = segments*m_
	m = 1-m'
	(r, g, b):(r', g', b'):_ = drop d (gradientStops ++ repeat (last gradientStops))

neutral :: Render ()
neutral = setSourceRGB 0.8 0.8 0.8

-- | If you want to render a heatmap, you should reserve a rectangle of this
-- size to contain it and its legend.
heatmapSizeRecommendation :: Board -> (Int, Int)
heatmapSizeRecommendation = fmap succ . bottleSizeRecommendation

-- | Good for when all your numbers are in the range [0,1]. The gradient used
-- for this makes it especially easy to spot values that are exactly 0 or
-- exactly 1.
heatmap01 :: Board -> PillContent -> [(Position, Double)] -> Render ()
heatmap01 = heatmapWith heatmapOptions01

-- | Good for distributions. The gradient used makes it easy to spot values
-- that are exactly 0 and scales the upper bound to be near the highest
-- probability given.
heatmap0Dyn :: Board -> PillContent -> [(Position, Double)] -> Render ()
heatmap0Dyn b pc heat = heatmap0Max (maximum (0:map snd heat)) b pc heat

-- | Good for distributions where you expect a probability to be at most a
-- certain maximum. The gradient used makes it easy to spot values that are
-- exactly 0, but doesn't treat values equal to the upper bound specially.
heatmap0Max :: Double -> Board -> PillContent -> [(Position, Double)] -> Render ()
heatmap0Max = heatmapWith . heatmapOptions0Max

-- | Good when you don't really know ahead of time how big your numbers will
-- be. Prints rounded versions of the min and max in the legend.
heatmapDyn :: Board -> PillContent -> [(Position, Double)] -> Render ()
heatmapDyn b pc heat = heatmapWith (heatmapOptionsDyn heat) b pc heat

-- | Good for when you know what you want the smallest and largest values in
-- your legend to be. The bounds are rounded before being printed in the
-- legend.
heatmapRange :: Double -> Double -> Board -> PillContent -> [(Position, Double)] -> Render ()
heatmapRange lo hi = heatmapWith (heatmapOptionsRange lo hi)

-- | See 'HeatmapOptions' below for more on exactly what knobs you can tweak
-- here.
heatmapWith :: HeatmapOptions -> Board -> PillContent -> [(Position, Double)] -> Render ()
heatmapWith ho b pc heat = do
	rectangle 0 0 w h
	neutral
	fill
	for_ heat $ \(Position x y, n) -> do
		rectangle (fromIntegral x+1) (fromIntegral y+1) 1 1
		hoGradient ho (rescale n)
		fill
	for_ (hoLegendLabels ho) $ \(l, r) -> do
		let skip = 1.5*hoPadding ho + hoLabelWidth ho
		withLinearPattern skip h (w-skip) h $ \pat -> do
			forZipWithM_ [0..] gradientStops $ \i (r, g, b) ->
				patternAddColorStopRGB pat (i / fromIntegral (length gradientStops - 1)) r g b
			rectangle skip (h-0.75) (w-2*skip) 0.5
			setSource pat
			fill
		setSourceRGB 0 0 0
		fitTexts $ tail [undefined
			, TextRequest { trX =                   0.5*hoPadding ho, trY = h-1, trW = hoLabelWidth ho, trH = 1, trText = l }
			, TextRequest { trX = w-hoLabelWidth ho-0.5*hoPadding ho, trY = h-1, trW = hoLabelWidth ho, trH = 1, trText = r }
			]
	bottle b
	lookaheadContent (width b) (height b) pc
	where
	(fromIntegral -> w, fromIntegral -> h) = heatmapSizeRecommendation b
	rescale = case hoRescale ho of
		Nothing -> id
		Just (lo, hi)
			| lo >= hi -> const 0.5
			| otherwise -> \n -> (n-lo) / (hi-lo)

data HeatmapOptions = HeatmapOptions
	{ hoRescale :: Maybe (Double, Double) -- ^ the 'hoGradient' generally clips its input to the range [0, 1]; if this is a 'Just', the inputs will be linearly scaled from the given range before 'hoGradient' is called
	, hoGradient :: Double -> Render () -- ^ an action that sets the background color for a given number
	, hoLegendLabels :: Maybe (String, String) -- ^ if this is a 'Just', a legend with the given labels will be drawn
	, hoPadding :: Double -- ^ this much space will be left between elements in the legend row
	, hoLabelWidth :: Double -- ^ labels will be fit into rectangles of height 1 and the given width at the top left and top right
	}

-- | Suitable options for when all your numbers are in the range [0, 1].
heatmapOptions01 :: HeatmapOptions
heatmapOptions01 = HeatmapOptions
	{ hoRescale = Nothing
	, hoGradient = bwGradient
	, hoPadding = 0.5
	, hoLabelWidth = 1
	, hoLegendLabels = Just ("0", "1")
	}

-- | Suitable options for probability distributions. Supply the largest
-- probability as the argument.
heatmapOptions0Max :: Double -> HeatmapOptions
heatmapOptions0Max p = HeatmapOptions
	{ hoRescale = Just (0, p)
	, hoGradient = bSaturatingGradient
	, hoPadding = 0.5
	, hoLabelWidth = 2
	, hoLegendLabels = Just ("0", showEFloat (Just 2) p "")
	}

-- | Suitable options for when you're not sure what range your numbers will be in.
heatmapOptionsDyn :: [(Position, Double)] -> HeatmapOptions
heatmapOptionsDyn = heatmapOptionsDyn' . map snd

-- | Suitable options for when you're not sure what range your numbers will be
-- in and don't want to pre-commit to their positions.
heatmapOptionsDyn' :: [Double] -> HeatmapOptions
heatmapOptionsDyn' = \case
	[] -> heatmapOptions01
	ns -> heatmapOptionsRange (minimum ns) (maximum ns)

-- | Suitable options for when your numbers are all in a specific range.
heatmapOptionsRange :: Double -> Double -> HeatmapOptions
heatmapOptionsRange lo hi
	| 0 <= lo && lo <= epsilon && 1-epsilon <= hi && hi <= 1 = heatmapOptions01
	| hi < lo = heatmapOptionsRange hi lo
	| otherwise = HeatmapOptions
		{ hoRescale = Just (lo, hi)
		, hoGradient = saturatingGradient
		, hoPadding = 0.5
		, hoLabelWidth = 2
		, hoLegendLabels = Just (showEFloat (Just 2) lo "", showEFloat (Just 2) hi "")
		}
	where epsilon = 0.01

data TextRequest = TextRequest
	{ trX, trY, trW, trH :: Double
	, trText :: String
	} deriving (Eq, Ord, Read, Show)

data Extents = Extents
	{ eTexts :: [(TextRequest, TextExtents)]
	, eFont :: FontExtents
	}

eFromRequests :: [TextRequest] -> Render Extents
eFromRequests trs = do
	setFontSize 1
	font <- fontExtents
	texts <- traverse (textExtents . trText) trs
	pure Extents
		{ eTexts = zip trs texts
		, eFont = font
		}

eFromRequest :: TextRequest -> Render Extents
eFromRequest = eFromRequests . pure

eScaling :: Extents -> Double
eScaling e = minimum . ((1/0):) $ do
	(tr, ext) <- eTexts e
	[trH tr / h, trW tr / textExtentsWidth ext]
	where
	h = fontExtentsAscent (eFont e) + fontExtentsDescent (eFont e)

eCenter :: Extents -> Render ()
eCenter e = do
	setFontMatrix (Matrix s 0 0 (-s) 0 0)
	setSourceRGB 0 0 0
	for_ (eTexts e) $ \(tr, te) -> do
		moveTo
			(trX tr - s * textExtentsXbearing te + (trW tr - s * textExtentsWidth te) / 2)
			(trY tr + (trH tr + dh) / 2)
		showText (trText tr)
	where
	s = eScaling e
	dh = s * (fontExtentsDescent (eFont e) - fontExtentsAscent (eFont e))

fitText :: Double -> Double -> Double -> Double -> String -> Render ()
fitText x y w h s = eFromRequest TextRequest { trX = x, trY = y, trW = w, trH = h, trText = s } >>= eCenter

fitTexts :: [TextRequest] -> Render ()
fitTexts = eFromRequests >=> eCenter
