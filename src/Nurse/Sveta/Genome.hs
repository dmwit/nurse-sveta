module Nurse.Sveta.Genome (
	Individual, Genome, newGenome, gClone,
	gSize, gConvWidth, gConvHeight,
	iEvaluate,
	gIndices, gAppend,
	gGetColorPattern, gGetShapePattern, gGetPatternScore,
	gSetColorPattern, gSetShapePattern, gSetPatternScore,
	IndividualSpec, iSpec, iFromSpec,
	gDump, gSketch,
	ConvolutionSize(..),
	WithSentinels(..),
	allColorsWithSentinels, allShapesWithSentinels,
	) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Builder
import Data.Coerce
import Data.Foldable
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.String
import Data.Vector (Vector)
import Dr.Mario.Model
import Foreign
import Foreign.C
import System.IO.Unsafe
import Text.Read

import qualified Data.Aeson.Encoding as A
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

foreign import ccall "boards_new" cxx_boards_new :: Ptr CChar -> Ptr CChar -> IO (Ptr Boards)
foreign import ccall unsafe "boards_delete" cxx_boards_delete :: Ptr Boards -> IO ()

foreign import ccall "genome_new" cxx_genome_new :: CInt -> CInt -> CInt -> CFloat -> IO (Ptr Genome)
foreign import ccall "genome_clone" cxx_genome_clone :: Ptr Genome -> IO (Ptr Genome)
foreign import ccall "&genome_delete" cxx_genome_delete :: FinalizerPtr Genome

foreign import ccall unsafe "genome_get_color_pattern" cxx_genome_get_color_pattern :: Ptr Genome -> CInt -> CInt -> CInt -> CInt -> IO CBool
foreign import ccall unsafe "genome_get_shape_pattern" cxx_genome_get_shape_pattern :: Ptr Genome -> CInt -> CInt -> CInt -> CInt -> IO CBool
foreign import ccall unsafe "genome_get_pattern_score" cxx_genome_get_pattern_score :: Ptr Genome -> CInt -> IO CFloat
foreign import ccall "genome_encode_patterns" cxx_genome_encode_patterns :: Ptr Genome -> Ptr CInt -> IO (Ptr CChar)
foreign import ccall "patterns_encoding_delete" cxx_patterns_encoding_delete :: Ptr CChar -> IO ()

foreign import ccall unsafe "genome_set_color_pattern" cxx_genome_set_color_pattern :: Ptr Genome -> CInt -> CInt -> CInt -> CInt -> CBool -> IO ()
foreign import ccall unsafe "genome_set_shape_pattern" cxx_genome_set_shape_pattern :: Ptr Genome -> CInt -> CInt -> CInt -> CInt -> CBool -> IO ()
foreign import ccall unsafe "genome_set_pattern_score" cxx_genome_set_pattern_score :: Ptr Genome -> CInt -> CFloat -> IO ()
foreign import ccall "genome_decode_patterns" cxx_genome_decode_patterns :: Ptr Genome -> Ptr CChar -> CInt -> IO ()

foreign import ccall unsafe "genome_size" cxx_genome_size :: Ptr Genome -> IO CInt
foreign import ccall unsafe "genome_conv_width" cxx_genome_conv_width :: Ptr Genome -> IO CInt
foreign import ccall unsafe "genome_conv_height" cxx_genome_conv_height :: Ptr Genome -> IO CInt

foreign import ccall "genome_indices" cxx_genome_indices :: Ptr Genome -> Ptr CInt -> CInt -> IO (Ptr Genome)
foreign import ccall "genome_append" cxx_genome_append :: Ptr Genome -> Ptr Genome -> IO (Ptr Genome)

foreign import ccall "genome_dump" cxx_genome_dump :: Ptr Genome -> IO ()
foreign import ccall "genome_sketch" cxx_genome_sketch :: Ptr Genome -> IO ()

foreign import ccall "evaluate" cxx_evaluate :: Ptr Genome -> Ptr Boards -> Ptr CFloat -> IO ()

newtype Boards = Boards (ForeignPtr Boards)
newtype Genome = Genome (ForeignPtr Genome)

-- | Intended invariant: for all @ind :: Individual@,
--
-- @and [gConvWidth g == csWidth cs && gConvHeight g == csHeight cs | (cs, g) <- HM.toList ind]@
type Individual = HashMap ConvolutionSize Genome

newGenome :: ConvolutionSize -> Int -> Float -> IO Genome
newGenome cs n p = gcGenome (cxx_genome_new (fromIntegral (csWidth cs)) (fromIntegral (csHeight cs)) (fromIntegral n) (realToFrac p))

gcGenome :: IO (Ptr Genome) -> IO Genome
gcGenome act = Genome <$> (act >>= newForeignPtr cxx_genome_delete)

gClone :: Genome -> IO Genome
gClone (Genome g) = withForeignPtr g (gcGenome . cxx_genome_clone)

gSize :: Genome -> Int
gSize (Genome g) = fromIntegral . unsafePerformIO $ withForeignPtr g cxx_genome_size

gConvWidth :: Genome -> Int
gConvWidth (Genome g) = fromIntegral . unsafePerformIO $ withForeignPtr g cxx_genome_conv_width

gConvHeight :: Genome -> Int
gConvHeight (Genome g) = fromIntegral . unsafePerformIO $ withForeignPtr g cxx_genome_conv_height

gGetColorPattern :: Genome -> Int -> WithSentinels Color -> Int -> Int -> Bool
gGetColorPattern (Genome g) n c w h = unsafePerformIO $ withForeignPtr g \cxx_g -> (0 /=) <$>
	cxx_genome_get_color_pattern cxx_g (fromIntegral n) (colorSentinelIndex c) (fromIntegral w) (fromIntegral h)

gGetShapePattern :: Genome -> Int -> WithSentinels Shape -> Int -> Int -> Bool
gGetShapePattern (Genome g) n s w h = unsafePerformIO $ withForeignPtr g \cxx_g -> (0 /=) <$>
	cxx_genome_get_shape_pattern cxx_g (fromIntegral n) (shapeSentinelIndex s) (fromIntegral w) (fromIntegral h)

gGetPatternScore :: Genome -> Int -> Float
gGetPatternScore (Genome g) n = unsafePerformIO $ withForeignPtr g \cxx_g -> realToFrac <$>
	cxx_genome_get_pattern_score cxx_g (fromIntegral n)

gEncodePatterns :: Genome -> [Word8]
gEncodePatterns (Genome g) = unsafePerformIO $
	withForeignPtr g \cxx_g ->
	alloca \cxx_length -> do
	cxx_bytes <- cxx_genome_encode_patterns cxx_g cxx_length
	len <- fromIntegral <$> peek cxx_length
	chars <- peekArray len cxx_bytes
	cxx_patterns_encoding_delete cxx_bytes
	pure (map fromIntegral chars)

gSetColorPattern :: Genome -> Int -> WithSentinels Color -> Int -> Int -> Bool -> IO ()
gSetColorPattern (Genome g) n c w h v = withForeignPtr g \cxx_g ->
	cxx_genome_set_color_pattern cxx_g (fromIntegral n) (colorSentinelIndex c) (fromIntegral w) (fromIntegral h) (fromIntegral (fromEnum v))

gSetShapePattern :: Genome -> Int -> WithSentinels Shape -> Int -> Int -> Bool -> IO ()
gSetShapePattern (Genome g) n s w h v = withForeignPtr g \cxx_g ->
	cxx_genome_set_shape_pattern cxx_g (fromIntegral n) (shapeSentinelIndex s) (fromIntegral w) (fromIntegral h) (fromIntegral (fromEnum v))

gSetPatternScore :: Genome -> Int -> Float -> IO ()
gSetPatternScore (Genome g) n v = withForeignPtr g \cxx_g ->
	cxx_genome_set_pattern_score cxx_g (fromIntegral n) (realToFrac v)

gDecodePatterns :: Genome -> [Word8] -> IO ()
gDecodePatterns (Genome g) bytes =
	withForeignPtr g \cxx_g ->
	withArrayLen (map fromIntegral bytes) \len cxx_bytes ->
	cxx_genome_decode_patterns cxx_g cxx_bytes (fromIntegral len)

gIndices :: Genome -> [Int] -> IO Genome
gIndices (Genome g) is =
	withArrayLen (fromIntegral <$> is) \isLen cxx_is ->
	withForeignPtr g \cxx_g ->
	gcGenome (cxx_genome_indices cxx_g cxx_is (fromIntegral isLen))

gAppend :: Genome -> Genome -> IO Genome
gAppend (Genome g) (Genome g') =
	withForeignPtr g \cxx_g ->
	withForeignPtr g' \cxx_g' ->
	gcGenome (cxx_genome_append cxx_g cxx_g')

gDump :: Genome -> IO ()
gDump (Genome g) = withForeignPtr g cxx_genome_dump

gSketch :: Genome -> IO ()
gSketch (Genome g) = withForeignPtr g cxx_genome_sketch

iEvaluate :: Individual -> Board -> Vector Board -> Vector Float
iEvaluate ind b bs = unsafePerformIO $ iEvaluateIO ind b bs

iEvaluateIO :: Individual -> Board -> Vector Board -> IO (Vector Float)
iEvaluateIO ind b bs | invalid = fail "iEvaluate only works on 8x16 boards (because the underlying C++ function does)"
	| otherwise =
		allocaArray 128 \cxx_base_board ->
		allocaArray (V.length bs) \cxx_out ->
		BS.useAsCStringLen diffsBS \(cxx_diffs, _len) -> do
			for_ [0..7] \x ->
				for_ [0..15] \y ->
					pokeElemOff cxx_base_board (x + 8*y) . fromIntegral . word8FromCell  . unsafeGet b $ Position x y
			cxx_bs <- cxx_boards_new cxx_base_board cxx_diffs
			hs_out <- MV.replicate n 0
			forM_ ind \(Genome g) -> withForeignPtr g \cxx_g -> do
				cxx_evaluate cxx_g cxx_bs cxx_out
				forM_ [0..n-1] \i -> flip (MV.modify hs_out) i . (+) =<< peekElemOff cxx_out i
			cxx_boards_delete cxx_bs
			coerce (V.unsafeFreeze @IO hs_out)
	where
	n = V.length bs
	invalidBoard b = width b /= 8 || height b /= 16
	invalid = invalidBoard b || any invalidBoard bs
	diffsBS = (BS.toStrict . toLazyByteString) diffsBuilder
	diffsBuilder = foldMap diff bs <> word8 0xfe
	diff b' = mconcat [overwrite x y c
		| x <- [0..7]
		, y <- [0..15]
		, let pos = Position x y
		      c = unsafeGet b' pos
		, c /= unsafeGet b pos
		] <> word8 0xff
	overwrite x y c = word8 (shiftL (fromIntegral x) 4 .|. fromIntegral y) <> word8 (word8FromCell c)
	word8FromCell = \case
		Empty -> 0x13
		Occupied col sh -> word8FromColor col .|. word8FromShape sh
	word8FromColor = colorIndex
	word8FromShape = (`shiftL` 2) . shapeIndex

data WithSentinels a = NonSentinel a | EmptySentinel | OutOfBoundsSentinel deriving (Eq, Ord, Read, Show)

instance Bounded a => Bounded (WithSentinels a) where
	minBound = NonSentinel minBound
	maxBound = OutOfBoundsSentinel

allColorsWithSentinels :: [WithSentinels Color]
allColorsWithSentinels = map NonSentinel [minBound..maxBound] ++ [EmptySentinel, OutOfBoundsSentinel]

allShapesWithSentinels :: [WithSentinels Shape]
allShapesWithSentinels = map NonSentinel [Virus, Disconnected, East, West] ++ [EmptySentinel, OutOfBoundsSentinel]

{-# Specialize colorIndex :: Color -> CInt #-}
{-# Specialize colorIndex :: Color -> Word8 #-}
colorIndex :: Num a => Color -> a
colorIndex = \case
	Blue -> 0
	Red -> 1
	Yellow -> 2

{-# Specialize shapeIndex :: Shape -> CInt #-}
{-# Specialize shapeIndex :: Shape -> Word8 #-}
shapeIndex :: Num a => Shape -> a
shapeIndex = \case
	Virus -> 0
	Disconnected -> 1
	North -> 1
	South -> 1
	East -> 2
	West -> 3

colorSentinelIndex :: WithSentinels Color -> CInt
colorSentinelIndex = \case
	NonSentinel a -> colorIndex a
	EmptySentinel -> 3
	OutOfBoundsSentinel -> 4

shapeSentinelIndex :: WithSentinels Shape -> CInt
shapeSentinelIndex = \case
	NonSentinel a -> shapeIndex a
	EmptySentinel -> 4
	OutOfBoundsSentinel -> 5

data ConvolutionSize = ConvolutionSize { csWidth, csHeight :: Int } deriving (Eq, Ord, Read, Show)

csToTuple :: ConvolutionSize -> (Int, Int)
csToTuple cs = (csWidth cs, csHeight cs)

csFromTuple :: (Int, Int) -> ConvolutionSize
csFromTuple (w, h) = ConvolutionSize { csWidth = w, csHeight = h }

instance ToJSON ConvolutionSize where
	toEncoding = toEncoding . csToTuple
	toJSON = toJSON . csToTuple

instance FromJSON ConvolutionSize where
	parseJSON v = csFromTuple <$> parseJSON v

instance ToJSONKey ConvolutionSize where
	toJSONKey = ToJSONKeyText
		(\cs -> fromString $ show (csWidth cs) ++ "x" ++ show (csHeight cs))
		(\cs -> fromString $ show (csWidth cs) ++ "x" ++ show (csHeight cs))

instance FromJSONKey ConvolutionSize where
	fromJSONKey = FromJSONKeyTextParser \t -> case T.breakOnAll "x" t of
		[(treadMaybe -> Just w, treadMaybe . T.drop 1 -> Just h)] -> pure $ ConvolutionSize w h
		_ -> typeMismatch "ConvolutionSize (a string of the form \"wxh\" where w and h are ints)" (toJSON t)
		where treadMaybe = readMaybe . T.unpack

instance Hashable ConvolutionSize where
	s `hashWithSalt` cs = s `hashWithSalt` (2059915244 :: Int) `hashWithSalt` csWidth cs `hashWithSalt` csHeight cs

data ConvolutionsSpec = ConvolutionsSpec
	{ csPatterns :: [Word8]
	, csScores :: [Float]
	} deriving (Eq, Ord, Read, Show)

instance ToJSON ConvolutionsSpec where
	toEncoding cs = A.list id
		$ (toEncoding . encodePrintable . csPatterns) cs
		: (map toEncoding . csScores) cs
	toJSON cs = toJSON
		$ (toJSON . encodePrintable . csPatterns) cs
		: (map toJSON . csScores) cs

instance FromJSON ConvolutionsSpec where
	parseJSON (Array vs) | V.length vs >= 1 = pure ConvolutionsSpec
		<*> (decodePrintable <$> parseJSON (vs V.! 0))
		<*> parseJSON (Array (V.drop 1 vs))
	parseJSON o = typeMismatch "ConvolutionsSpec (an array with a string and some floats)" o

type IndividualSpec = HashMap ConvolutionSize ConvolutionsSpec

iSpec :: Individual -> IndividualSpec
iSpec = fmap \g -> ConvolutionsSpec
	{ csPatterns = gEncodePatterns g
	, csScores = gGetPatternScore g <$> [0..gSize g-1]
	}

iFromSpec :: IndividualSpec -> IO Individual
iFromSpec = HM.traverseWithKey \sz conv -> do
	let len = length (csScores conv)
	g <- newGenome sz len 0
	gDecodePatterns g (csPatterns conv)
	zipWithM_ (gSetPatternScore g) [0..] (csScores conv)
	pure g

-- encodePrintable and decodePrintable convert between unconstrained byte
-- sequences and sequences of bytes that JSON can represent in one byte each:
-- " !#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~"
-- this gets us about logBase 93 256 = 1.2234 bytes/byte on average
encodePrintable :: [Word8] -> String
encodePrintable = map toEnum . expand . contract where
	contract [] = 0
	contract (w:ws) = toInteger w .|. shiftL (contract ws) 8

	expand 0 = []
	expand n = byte : expand q where
		(q, r) = n `quotRem` 93
		byte = 32 + fromInteger r
			+ (if r <  2 then 0 else 1)
			+ (if r < 59 then 0 else 1)

decodePrintable :: String -> [Word8]
decodePrintable = expand . contract . map fromEnum where
	contract [] = 0
	contract (w:ws) = toInteger byte + 93 * contract ws where
		byte = w - 32 - (if w < 34 then 0 else 1) - (if w < 93 then 0 else 1)

	expand 0 = []
	expand n = fromInteger n : expand (shiftR n 8)
