module Nurse.Sveta.Genome (
	Genome, newGenome, gClone,
	gSize, gConvWidth, gConvHeight,
	gEvaluate,
	gIndices, gAppend,
	gGetColorPattern, gGetShapePattern, gGetPatternScore,
	gSetColorPattern, gSetShapePattern, gSetPatternScore,
	gDump, gSketch,
	WithSentinels(..),
	) where

import Data.ByteString.Builder
import Data.Foldable
import Data.Vector (Vector)
import Dr.Mario.Model
import Foreign
import Foreign.C
import System.IO.Unsafe

import qualified Data.ByteString as BS
import qualified Data.Vector as V

foreign import ccall "boards_new" cxx_boards_new :: Ptr CChar -> Ptr CChar -> IO (Ptr Boards)
foreign import ccall unsafe "boards_delete" cxx_boards_delete :: Ptr Boards -> IO ()

foreign import ccall "genome_new" cxx_genome_new :: CInt -> CInt -> CInt -> CFloat -> IO (Ptr Genome)
foreign import ccall "genome_clone" cxx_genome_clone :: Ptr Genome -> IO (Ptr Genome)
foreign import ccall "&genome_delete" cxx_genome_delete :: FinalizerPtr Genome

foreign import ccall unsafe "genome_get_color_pattern" cxx_genome_get_color_pattern :: Ptr Genome -> CInt -> CInt -> CInt -> CInt -> IO CBool
foreign import ccall unsafe "genome_get_shape_pattern" cxx_genome_get_shape_pattern :: Ptr Genome -> CInt -> CInt -> CInt -> CInt -> IO CBool
foreign import ccall unsafe "genome_get_pattern_score" cxx_genome_get_pattern_score :: Ptr Genome -> CInt -> IO CFloat

foreign import ccall unsafe "genome_set_color_pattern" cxx_genome_set_color_pattern :: Ptr Genome -> CInt -> CInt -> CInt -> CInt -> CBool -> IO ()
foreign import ccall unsafe "genome_set_shape_pattern" cxx_genome_set_shape_pattern :: Ptr Genome -> CInt -> CInt -> CInt -> CInt -> CBool -> IO ()
foreign import ccall unsafe "genome_set_pattern_score" cxx_genome_set_pattern_score :: Ptr Genome -> CInt -> CFloat -> IO ()

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

newGenome :: Int -> Int -> Int -> Float -> IO Genome
newGenome w h n p = gcGenome (cxx_genome_new (fromIntegral w) (fromIntegral h) (fromIntegral n) (realToFrac p))

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

gSetColorPattern :: Genome -> Int -> WithSentinels Color -> Int -> Int -> Bool -> IO ()
gSetColorPattern (Genome g) n c w h v = withForeignPtr g \cxx_g ->
	cxx_genome_set_color_pattern cxx_g (fromIntegral n) (colorSentinelIndex c) (fromIntegral w) (fromIntegral h) (fromIntegral (fromEnum v))

gSetShapePattern :: Genome -> Int -> WithSentinels Shape -> Int -> Int -> Bool -> IO ()
gSetShapePattern (Genome g) n s w h v = withForeignPtr g \cxx_g ->
	cxx_genome_set_shape_pattern cxx_g (fromIntegral n) (shapeSentinelIndex s) (fromIntegral w) (fromIntegral h) (fromIntegral (fromEnum v))

gSetPatternScore :: Genome -> Int -> Float -> IO ()
gSetPatternScore (Genome g) n v = withForeignPtr g \cxx_g ->
	cxx_genome_set_pattern_score cxx_g (fromIntegral n) (realToFrac v)

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

gEvaluate :: Genome -> Board -> Vector Board -> Vector Float
gEvaluate g b bs = unsafePerformIO $ gEvaluateIO g b bs

gEvaluateIO :: Genome -> Board -> Vector Board -> IO (Vector Float)
gEvaluateIO (Genome g) b bs | invalid = fail "gEvaluate only works on 8x16 boards (because the underlying C++ function does)"
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
