module Nurse.Sveta.Genome (
	Genome, newGenome,
	gSize, gConvWidth, gConvHeight,
	gEvaluate,
	gIndices, gAppend,
	gDump, gSketch,
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
foreign import ccall "boards_delete" cxx_boards_delete :: Ptr Boards -> IO ()

foreign import ccall "genome_new" cxx_genome_new :: CInt -> CInt -> CInt -> CFloat -> IO (Ptr Genome)
foreign import ccall "&genome_delete" cxx_genome_delete :: FinalizerPtr Genome
foreign import ccall "genome_size" cxx_genome_size :: Ptr Genome -> IO CInt
foreign import ccall "genome_conv_width" cxx_genome_conv_width :: Ptr Genome -> IO CInt
foreign import ccall "genome_conv_height" cxx_genome_conv_height :: Ptr Genome -> IO CInt
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

gSize :: Genome -> Int
gSize (Genome g) = fromIntegral . unsafePerformIO $ withForeignPtr g cxx_genome_size

gConvWidth :: Genome -> Int
gConvWidth (Genome g) = fromIntegral . unsafePerformIO $ withForeignPtr g cxx_genome_conv_width

gConvHeight :: Genome -> Int
gConvHeight (Genome g) = fromIntegral . unsafePerformIO $ withForeignPtr g cxx_genome_conv_height

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
	word8FromColor = \case
		Blue -> 0
		Red -> 1
		Yellow -> 2
	word8FromShape = \case
		Virus -> 0
		Disconnected -> 4
		North -> 4
		South -> 4
		East -> 8
		West -> 12
