module Nurse.Sveta.Chromosome (
	Chromosome, newChromosome, cClone,
	cSize, cConvWidth, cConvHeight,
	cEvaluate,
	cIndices, cAppend,
	cGetColorPattern, cGetShapePattern, cGetPatternScore,
	cSetColorPattern, cSetShapePattern, cSetPatternScore,
	ChromosomeSpec, cSpec, cFromSpec,
	cDump, cSketch,
	WithSentinels(..),
	) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Builder
import Data.Foldable
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
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

foreign import ccall "boards_new" cxx_boards_new :: Ptr CChar -> Ptr CChar -> IO (Ptr Boards)
foreign import ccall unsafe "boards_delete" cxx_boards_delete :: Ptr Boards -> IO ()

foreign import ccall "chromosome_new" cxx_chromosome_new :: CInt -> CInt -> CInt -> CFloat -> IO (Ptr Chromosome)
foreign import ccall "chromosome_clone" cxx_chromosome_clone :: Ptr Chromosome -> IO (Ptr Chromosome)
foreign import ccall "&chromosome_delete" cxx_chromosome_delete :: FinalizerPtr Chromosome

foreign import ccall unsafe "chromosome_get_color_pattern" cxx_chromosome_get_color_pattern :: Ptr Chromosome -> CInt -> CInt -> CInt -> CInt -> IO CBool
foreign import ccall unsafe "chromosome_get_shape_pattern" cxx_chromosome_get_shape_pattern :: Ptr Chromosome -> CInt -> CInt -> CInt -> CInt -> IO CBool
foreign import ccall unsafe "chromosome_get_pattern_score" cxx_chromosome_get_pattern_score :: Ptr Chromosome -> CInt -> IO CFloat
foreign import ccall "chromosome_encode_patterns" cxx_chromosome_encode_patterns :: Ptr Chromosome -> Ptr CInt -> IO (Ptr CChar)
foreign import ccall "patterns_encoding_delete" cxx_patterns_encoding_delete :: Ptr CChar -> IO ()

foreign import ccall unsafe "chromosome_set_color_pattern" cxx_chromosome_set_color_pattern :: Ptr Chromosome -> CInt -> CInt -> CInt -> CInt -> CBool -> IO ()
foreign import ccall unsafe "chromosome_set_shape_pattern" cxx_chromosome_set_shape_pattern :: Ptr Chromosome -> CInt -> CInt -> CInt -> CInt -> CBool -> IO ()
foreign import ccall unsafe "chromosome_set_pattern_score" cxx_chromosome_set_pattern_score :: Ptr Chromosome -> CInt -> CFloat -> IO ()
foreign import ccall "chromosome_decode_patterns" cxx_chromosome_decode_patterns :: Ptr Chromosome -> Ptr CChar -> CInt -> IO ()

foreign import ccall unsafe "chromosome_size" cxx_chromosome_size :: Ptr Chromosome -> IO CInt
foreign import ccall unsafe "chromosome_conv_width" cxx_chromosome_conv_width :: Ptr Chromosome -> IO CInt
foreign import ccall unsafe "chromosome_conv_height" cxx_chromosome_conv_height :: Ptr Chromosome -> IO CInt

foreign import ccall "chromosome_indices" cxx_chromosome_indices :: Ptr Chromosome -> Ptr CInt -> CInt -> IO (Ptr Chromosome)
foreign import ccall "chromosome_append" cxx_chromosome_append :: Ptr Chromosome -> Ptr Chromosome -> IO (Ptr Chromosome)

foreign import ccall "chromosome_dump" cxx_chromosome_dump :: Ptr Chromosome -> IO ()
foreign import ccall "chromosome_sketch" cxx_chromosome_sketch :: Ptr Chromosome -> IO ()

foreign import ccall "evaluate" cxx_evaluate :: Ptr Chromosome -> Ptr Boards -> Ptr CFloat -> IO ()

newtype Boards = Boards (ForeignPtr Boards)
newtype Chromosome = Chromosome (ForeignPtr Chromosome)

newChromosome :: Int -> Int -> Int -> Float -> IO Chromosome
newChromosome w h n p = gcChromosome (cxx_chromosome_new (fromIntegral w) (fromIntegral h) (fromIntegral n) (realToFrac p))

gcChromosome :: IO (Ptr Chromosome) -> IO Chromosome
gcChromosome act = Chromosome <$> (act >>= newForeignPtr cxx_chromosome_delete)

cClone :: Chromosome -> IO Chromosome
cClone (Chromosome g) = withForeignPtr g (gcChromosome . cxx_chromosome_clone)

cSize :: Chromosome -> Int
cSize (Chromosome g) = fromIntegral . unsafePerformIO $ withForeignPtr g cxx_chromosome_size

cConvWidth :: Chromosome -> Int
cConvWidth (Chromosome g) = fromIntegral . unsafePerformIO $ withForeignPtr g cxx_chromosome_conv_width

cConvHeight :: Chromosome -> Int
cConvHeight (Chromosome g) = fromIntegral . unsafePerformIO $ withForeignPtr g cxx_chromosome_conv_height

cGetColorPattern :: Chromosome -> Int -> WithSentinels Color -> Int -> Int -> Bool
cGetColorPattern (Chromosome g) n c w h = unsafePerformIO $ withForeignPtr g \cxx_g -> (0 /=) <$>
	cxx_chromosome_get_color_pattern cxx_g (fromIntegral n) (colorSentinelIndex c) (fromIntegral w) (fromIntegral h)

cGetShapePattern :: Chromosome -> Int -> WithSentinels Shape -> Int -> Int -> Bool
cGetShapePattern (Chromosome g) n s w h = unsafePerformIO $ withForeignPtr g \cxx_g -> (0 /=) <$>
	cxx_chromosome_get_shape_pattern cxx_g (fromIntegral n) (shapeSentinelIndex s) (fromIntegral w) (fromIntegral h)

cGetPatternScore :: Chromosome -> Int -> Float
cGetPatternScore (Chromosome g) n = unsafePerformIO $ withForeignPtr g \cxx_g -> realToFrac <$>
	cxx_chromosome_get_pattern_score cxx_g (fromIntegral n)

cEncodePatterns :: Chromosome -> [Word8]
cEncodePatterns (Chromosome g) = unsafePerformIO $
	withForeignPtr g \cxx_g ->
	alloca \cxx_length -> do
	cxx_bytes <- cxx_chromosome_encode_patterns cxx_g cxx_length
	len <- fromIntegral <$> peek cxx_length
	chars <- peekArray len cxx_bytes
	cxx_patterns_encoding_delete cxx_bytes
	pure (map fromIntegral chars)

cSetColorPattern :: Chromosome -> Int -> WithSentinels Color -> Int -> Int -> Bool -> IO ()
cSetColorPattern (Chromosome g) n c w h v = withForeignPtr g \cxx_g ->
	cxx_chromosome_set_color_pattern cxx_g (fromIntegral n) (colorSentinelIndex c) (fromIntegral w) (fromIntegral h) (fromIntegral (fromEnum v))

cSetShapePattern :: Chromosome -> Int -> WithSentinels Shape -> Int -> Int -> Bool -> IO ()
cSetShapePattern (Chromosome g) n s w h v = withForeignPtr g \cxx_g ->
	cxx_chromosome_set_shape_pattern cxx_g (fromIntegral n) (shapeSentinelIndex s) (fromIntegral w) (fromIntegral h) (fromIntegral (fromEnum v))

cSetPatternScore :: Chromosome -> Int -> Float -> IO ()
cSetPatternScore (Chromosome g) n v = withForeignPtr g \cxx_g ->
	cxx_chromosome_set_pattern_score cxx_g (fromIntegral n) (realToFrac v)

cDecodePatterns :: Chromosome -> [Word8] -> IO ()
cDecodePatterns (Chromosome g) bytes =
	withForeignPtr g \cxx_g ->
	withArrayLen (map fromIntegral bytes) \len cxx_bytes ->
	cxx_chromosome_decode_patterns cxx_g cxx_bytes (fromIntegral len)

cIndices :: Chromosome -> [Int] -> IO Chromosome
cIndices (Chromosome g) is =
	withArrayLen (fromIntegral <$> is) \isLen cxx_is ->
	withForeignPtr g \cxx_g ->
	gcChromosome (cxx_chromosome_indices cxx_g cxx_is (fromIntegral isLen))

cAppend :: Chromosome -> Chromosome -> IO Chromosome
cAppend (Chromosome g) (Chromosome g') =
	withForeignPtr g \cxx_g ->
	withForeignPtr g' \cxx_g' ->
	gcChromosome (cxx_chromosome_append cxx_g cxx_g')

cDump :: Chromosome -> IO ()
cDump (Chromosome g) = withForeignPtr g cxx_chromosome_dump

cSketch :: Chromosome -> IO ()
cSketch (Chromosome g) = withForeignPtr g cxx_chromosome_sketch

cEvaluate :: Chromosome -> Board -> Vector Board -> Vector Float
cEvaluate g b bs = unsafePerformIO $ cEvaluateIO g b bs

cEvaluateIO :: Chromosome -> Board -> Vector Board -> IO (Vector Float)
cEvaluateIO (Chromosome g) b bs | invalid = fail "cEvaluate only works on 8x16 boards (because the underlying C++ function does)"
	| otherwise =
		withForeignPtr g \cxx_g ->
		allocaArray 128 \cxx_base_board ->
		allocaArray (V.length bs) \cxx_out ->
		BS.useAsCStringLen diffsBS \(cxx_diffs, _len) -> do
			for_ [0..7] \x ->
				for_ [0..15] \y ->
					pokeElemOff cxx_base_board (x + 8*y) . fromIntegral . word8FromCell  . unsafeGet b $ Position x y
			cxx_bs <- cxx_boards_new cxx_base_board cxx_diffs
			cxx_evaluate cxx_g cxx_bs cxx_out
			cxx_boards_delete cxx_bs
			V.iforM bs \i _ -> realToFrac <$> peekElemOff cxx_out i
	where
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

instance ToJSON Chromosome where
	toJSON = toJSON . cSpec
	toEncoding = toEncoding . cSpec

type ChromosomeSpec = Map ConvolutionSize ConvolutionsSpec

cSpec :: Chromosome -> ChromosomeSpec
cSpec g = M.singleton
	ConvolutionSize
		{ csWidth = cConvWidth g
		, csHeight = cConvHeight g
		}
	ConvolutionsSpec
		{ csPatterns = cEncodePatterns g
		, csScores = cGetPatternScore g <$> [0..cSize g-1]
		}

cFromSpec :: ChromosomeSpec -> IO Chromosome
cFromSpec gs = case M.toList gs of
	[(sz, conv)] -> do
		let len = length (csScores conv)
		g <- newChromosome (csWidth sz) (csHeight sz) len 0
		cDecodePatterns g (csPatterns conv)
		cSetPatternScore g (len-1) 1 -- avoid rescaling until we're done
		zipWithM_ (cSetPatternScore g) [0..] (csScores conv)
		pure g
	_ -> fail $ "Building a chromosome with more (or fewer) than one size of convolution is not (yet) supported. (Saw " ++ show (M.size gs) ++ " sizes.)"

-- encodePrintable and decodePrintable convert between unconstrained byte
-- sequences and sequences of bytes that JSON can represent in one byte each:
-- " !#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~"
-- this gets us about locBase 93 256 = 1.2234 bytes/byte on average
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
