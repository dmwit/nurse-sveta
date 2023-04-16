module Nurse.Sveta.Cairo (
	initMath, bottleSizeRecommendation, heatmapSizeRecommendation,
	heatmap01,
	bottleWithLookahead, bottle, bottleMaybeLookahead,
	bottleOutline, bottleContent, lookahead,
	fitText, fitTexts, TextRequest(..),
	pill, cell, setColor,
	) where

import Control.Applicative
import Control.Monad
import Data.Fixed
import Data.Foldable
import Data.Monoid
import GI.Cairo.Render hiding (RectangleInt(..))
import GI.Cairo.Render.Matrix (Matrix(..))
import Dr.Mario.Model
import Nurse.Sveta.Util

-- | Under normal circumstances, cairo coordinates are (0,0) at the top left,
-- with x increasing to the right and y increasing down. This converts to (0,0)
-- at the bottom left, with x increasing to the right and y increasing down.
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
lookahead_ w h xMid l r = do
	cell  xMid    (h+1.5) (Occupied l West)
	cell (xMid+1) (h+1.5) (Occupied r East)

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

gradient :: Double -> Render ()
gradient n
	| n <= 0 = setSourceRGB 0 0 0
	| n >= 1 = setSourceRGB 1 1 1
	| otherwise = setSourceRGB (m*r + m'*r') (m*g + m'*g') (m*b + m'*b')
	where
	segments = fromIntegral (length gradientStops) - 1
	(d, m_) = n `divMod'` recip segments
	m = segments*m_
	m' = 1-m
	(r, g, b):(r', g', b'):_ = drop d gradientStops

neutral :: Render ()
neutral = setSourceRGB 0.8 0.8 0.8

-- | If you want to render a heatmap, you should reserve a rectangle of this
-- size to contain it and its legend.
heatmapSizeRecommendation :: Board -> (Int, Int)
heatmapSizeRecommendation = fmap succ . bottleSizeRecommendation

-- | Render a board with some colored background indicating the numbers in the
-- given list, which should all be in the range [0,1] and at unique positions
-- that are in-bounds for the board. A legend will be placed at the top.
heatmap01 :: Board -> [(Position, Double)] -> Render ()
heatmap01 b heat = do
	rectangle 0 0 w h
	neutral
	fill
	for_ heat $ \(Position x y, n) -> do
		rectangle (fromIntegral x+1) (fromIntegral y+1) 1 1
		gradient n
		fill
	withLinearPattern 1.1 h (w-1.1) h $ \pat -> do
		forZipWithM_ [0..] gradientStops $ \i (r, g, b) ->
			patternAddColorStopRGB pat (i / fromIntegral (length gradientStops - 1)) r g b
		rectangle 1.1 (h-0.75) (w-2.2) 0.5
		setSource pat
		fill
	fitTexts $ tail [undefined
		, TextRequest { trx = 0  , try = h-1, trw = 1, trh = 1, trText = "0" }
		, TextRequest { trx = w-1, try = h-1, trw = 1, trh = 1, trText = "1" }
		]
	bottle b
	where
	(fromIntegral -> w, fromIntegral -> h) = heatmapSizeRecommendation b

data TextRequest = TextRequest
	{ trx, try, trw, trh :: Double
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
	[trh tr / h, trw tr / textExtentsWidth ext]
	where
	h = fontExtentsAscent (eFont e) + fontExtentsDescent (eFont e)

eCenter :: Extents -> Render ()
eCenter e = do
	setFontMatrix (Matrix s 0 0 (-s) 0 0)
	setSourceRGB 0 0 0
	for_ (eTexts e) $ \(tr, te) -> do
		moveTo
			(trx tr - s * textExtentsXbearing te + (trw tr - s * textExtentsWidth te) / 2)
			(try tr + (trh tr + dh) / 2)
		showText (trText tr)
	where
	s = eScaling e
	dh = s * (fontExtentsDescent (eFont e) - fontExtentsAscent (eFont e))

fitText :: Double -> Double -> Double -> Double -> String -> Render ()
fitText x y w h s = eFromRequest TextRequest { trx = x, try = y, trw = w, trh = h, trText = s } >>= eCenter

fitTexts :: [TextRequest] -> Render ()
fitTexts = eFromRequests >=> eCenter
