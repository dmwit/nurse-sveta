module Ms.Mendel.CXX.Cooked
	( Boards, newBoards, bSize
	, PatternsTemplate, newPatternsTemplate, ptWidth, ptHeight, ptSize
	, ptGetColor, ptGetShape, ptSetColor, ptSetShape, ptEncode, ptDecode
	, Patterns, newPatterns, pSize, pWidth, pHeight, pMirroringSize, pColoringSize, pReplicationSize
	, Scores, newScores, sGet, sSet, sFreeze, sUnsafeFreeze
	, Tensor, newTensor, tUnpackF, tUnpackI, tThaw, tUnsafeThaw, tTanh, tAdd, tScale
	, bpMatchFull, tSummarizeMatch, tScore, bpMatchSummary, bptEvaluate, bptEvaluateSync
	, WithSentinels(..), allColorsWithSentinels, allShapesWithSentinels
	, bDump, ptDump, pDump, sDump, tDump
	, bSketch, ptSketch, pSketch, sSketch, tSketch
	) where

import Data.ByteString.Builder
import Data.ByteString (ByteString)
import Foreign
import Ms.Mendel.CXX.Raw as C
import Nurse.Sveta.Util
import System.IO.Unsafe

import qualified Data.ByteString as BS
import qualified Data.Vector as V

---------- Boards ----------

newBoards :: Board -> Vector Board -> Boards
newBoards b bs = newWrapper Boards boards_delete_ptr do
	when invalid $ fail "newBoards only works on 8x16 boards (because the underlying C++ function does)"
	allocaArray 128 \base_board ->
		BS.useAsCStringLen diffsBS \(diffs, _len) -> do
			print diffsBS
			for_ [0..7] \x ->
				for_ [0..15] \y ->
					pokeElemOff base_board (x + 8*y) . fromIntegral . word8FromCell  . unsafeGet b $ Position x y
			boards_new base_board diffs
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

bSize :: Boards -> Int64
bSize (Boards bs) = unsafePerformIO (withForeignPtr bs boards_size)

bDump :: Boards -> IO ()
bDump (Boards bs) = withForeignPtr bs boards_dump

bSketch :: Boards -> IO ()
bSketch (Boards bs) = withForeignPtr bs boards_sketch

instance Show Boards where show _ = "<TODO: instance Show Boards>"

---------- PatternsTemplate ----------

newPatternsTemplate :: Int64 -> Int64 -> Int64 -> IO PatternsTemplate
newPatternsTemplate w h n = newWrapperIO PatternsTemplate patterns_template_delete_ptr =<< patterns_template_new w h n

ptWidth :: PatternsTemplate -> Int64
ptWidth (PatternsTemplate pt) = unsafePerformIO (withForeignPtr pt patterns_template_conv_width)

ptHeight :: PatternsTemplate -> Int64
ptHeight (PatternsTemplate pt) = unsafePerformIO (withForeignPtr pt patterns_template_conv_height)

ptSize :: PatternsTemplate -> Int64
ptSize (PatternsTemplate pt) = unsafePerformIO (withForeignPtr pt patterns_template_size)

ptGetColor :: PatternsTemplate -> Int64 -> WithSentinels Color -> Position -> IO Bool
ptGetColor (PatternsTemplate pt) n c (Position x y) = toBool <$> withForeignPtr pt \ptRaw -> patterns_template_get_color_pattern ptRaw n (colorSentinelIndex c) (fromIntegral x) (fromIntegral y)

ptGetShape :: PatternsTemplate -> Int64 -> WithSentinels Shape -> Position -> IO Bool
ptGetShape (PatternsTemplate pt) n s (Position x y) = toBool <$> withForeignPtr pt \ptRaw -> patterns_template_get_shape_pattern ptRaw n (shapeSentinelIndex s) (fromIntegral x) (fromIntegral y)

ptSetColor :: PatternsTemplate -> Int64 -> WithSentinels Color -> Position -> Bool -> IO ()
ptSetColor (PatternsTemplate pt) n c (Position x y) v = withForeignPtr pt \ptRaw -> patterns_template_set_color_pattern ptRaw n (colorSentinelIndex c) (fromIntegral x) (fromIntegral y) (fromBool v)

ptSetShape :: PatternsTemplate -> Int64 -> WithSentinels Shape -> Position -> Bool -> IO ()
ptSetShape (PatternsTemplate pt) n s (Position x y) v = withForeignPtr pt \ptRaw -> patterns_template_set_shape_pattern ptRaw n (shapeSentinelIndex s) (fromIntegral x) (fromIntegral y) (fromBool v)

ptDecode :: ByteString -> IO PatternsTemplate
ptDecode bs = BS.useAsCStringLen bs \(bsRaw, len) -> newWrapperIO PatternsTemplate patterns_template_delete_ptr =<< patterns_template_decode bsRaw (fromIntegral len)

ptEncode :: PatternsTemplate -> IO ByteString
ptEncode (PatternsTemplate pt) = withForeignPtr pt \ptRaw -> alloca \lenPtr -> do
	bsRaw <- patterns_template_encode ptRaw lenPtr
	len <- peek lenPtr
	bs <- BS.packCStringLen (bsRaw, fromIntegral len)
	patterns_template_encoding_delete bsRaw
	pure bs

ptDump :: PatternsTemplate -> IO ()
ptDump (PatternsTemplate pt) = withForeignPtr pt patterns_template_dump

ptSketch :: PatternsTemplate -> IO ()
ptSketch (PatternsTemplate pt) = withForeignPtr pt patterns_template_sketch

---------- Patterns ----------

newPatterns :: PatternsTemplate -> Bool -> Bool -> IO Patterns
newPatterns (PatternsTemplate pt) mirroring coloring = newWrapperIO Patterns patterns_delete_ptr =<< withForeignPtr pt \ptRaw -> patterns_new ptRaw (fromBool mirroring) (fromBool coloring)

pSize, pWidth, pHeight, pMirroringSize, pColoringSize, pReplicationSize :: Patterns -> Int64
[pSize, pWidth, pHeight, pMirroringSize, pColoringSize, pReplicationSize] = map pStatistic
	[patterns_size, patterns_conv_width, patterns_conv_height, patterns_mirroring_size, patterns_coloring_size, patterns_replication_size]
	where pStatistic f (Patterns p) = unsafePerformIO (withForeignPtr p f)

pDump :: Patterns -> IO ()
pDump (Patterns p) = withForeignPtr p patterns_dump

pSketch :: Patterns -> IO ()
pSketch (Patterns p) = withForeignPtr p patterns_sketch

instance Show Patterns where show _ = "<TODO: instance Show Patterns>"

---------- Scores ----------

newScores :: Vector Float -> IO Scores
newScores vals = allocaArray n \valsRaw -> do
	V.iforM_ vals (pokeElemOff valsRaw)
	tRaw <- scores_new valsRaw (fromIntegral n)
	Scores <$> gcTensorIO tRaw
	where n = V.length vals

sGet :: Scores -> Int -> IO Float
sGet (Scores (Tensor t)) i = withForeignPtr t \tRaw -> scores_get tRaw (fromIntegral i)

sSet :: Scores -> Int -> Float -> IO ()
sSet (Scores (Tensor t)) i val = withForeignPtr t \tRaw -> scores_set tRaw (fromIntegral i) val

-- | You must not mutate the given 'Scores' again. If you're not sure, use
-- 'sFreeze' instead.
sUnsafeFreeze :: Scores -> IO Tensor
sUnsafeFreeze (Scores t) = pure t

sFreeze :: Scores -> IO Tensor
sFreeze (Scores t) = gcTensorWithIO tensor_clone t

sDump :: Scores -> IO ()
sDump (Scores t) = tDump t

sSketch :: Scores -> IO ()
sSketch (Scores t) = tSketch t

---------- Tensor ----------

gcTensorIO :: Ptr Tensor -> IO Tensor
gcTensorIO = newWrapperIO Tensor tensor_delete_ptr

gcTensorWithIO :: (Ptr Tensor -> IO (Ptr Tensor)) -> Tensor -> IO Tensor
gcTensorWithIO f (Tensor t) = gcTensorIO =<< withForeignPtr t f

gcTensor :: IO (Ptr Tensor) -> Tensor
gcTensor = newWrapper Tensor tensor_delete_ptr

gcTensorWith :: (Ptr Tensor -> IO (Ptr Tensor)) -> Tensor -> Tensor
gcTensorWith f (Tensor t) = gcTensor (withForeignPtr t f)

gcTensorWith2 :: (Ptr Tensor -> Ptr Tensor -> IO (Ptr Tensor)) -> Tensor -> Tensor -> Tensor
gcTensorWith2 f (Tensor t) (Tensor t') = gcTensor $
	withForeignPtr t \tRaw ->
	withForeignPtr t' \tRaw' ->
	f tRaw tRaw'

newTensor :: Vector Float -> Tensor
newTensor = unsafePerformIO . coerce newScores

tUnpackF :: Tensor -> Int -> Vector Float
tUnpackF (Tensor t) n = unsafePerformIO $
	withForeignPtr t \tRaw ->
	allocaArray n \out -> do
	float_tensor_to_cpu tRaw out (fromIntegral n)
	V.generateM n (peekElemOff out)

tUnpackI :: Tensor -> Int -> Vector Int64
tUnpackI (Tensor t) n = unsafePerformIO $
	withForeignPtr t \tRaw ->
	allocaArray n \out -> do
	int_tensor_to_cpu tRaw out (fromIntegral n)
	V.generateM n (peekElemOff out)

-- | You must not read from the given 'Tensor' again. If you're not sure, use
-- 'sThaw' instead.
tUnsafeThaw :: Tensor -> IO Scores
tUnsafeThaw = pure . Scores

tThaw :: Tensor -> IO Scores
tThaw = fmap Scores . sFreeze . Scores

tTanh :: Tensor -> Tensor
tTanh = gcTensorWith tensor_tanh

tAdd :: Tensor -> Tensor -> Tensor
tAdd = gcTensorWith2 tensor_add

tScale :: Float -> Tensor -> Tensor
tScale = gcTensorWith . tensor_scale

tDump :: Tensor -> IO ()
tDump (Tensor t) = withForeignPtr t tensor_dump

tSketch :: Tensor -> IO ()
tSketch (Tensor t) = withForeignPtr t tensor_sketch

instance Show Tensor where show _ = "<TODO: instance Show Tensor>"

---------- the functions that make this worth it ----------

-- _ -> _ -> n x r x p x w x h @GPU_BOOL_REP where
-- n = bSize
-- r = pReplicationSize
-- p = pSize
-- w = 8 + pWidth - 1
-- h = 16 + pHeight - 1
bpMatchFull :: Boards -> Patterns -> Tensor
bpMatchFull (Boards bs) (Patterns ps) = gcTensor $
	withForeignPtr bs \bsRaw ->
	withForeignPtr ps \psRaw ->
	match_full bsRaw psRaw

-- n x r x p x w x h @GPU_BOOL_REP -> n x p @GPU_I64
tSummarizeMatch :: Tensor -> Tensor
tSummarizeMatch = gcTensorWith summarize_match

-- n x p @GPU_I64 -> p @GPU_FLOAT -> n @GPU_FLOAT
tScore :: Tensor -> Tensor -> Tensor
tScore = gcTensorWith2 score

-- _ -> _ -> n x p @GPU_I64
-- summarizeMatch . matchFull
bpMatchSummary :: Boards -> Patterns -> Tensor
bpMatchSummary (Boards bs) (Patterns ps) = gcTensor $
	withForeignPtr bs \bsRaw ->
	withForeignPtr ps \psRaw ->
	match_summary bsRaw psRaw

-- _ -> _ -> p @GPU_FLOAT -> n @GPU_FLOAT
-- score . matchSummary
bptEvaluate :: Boards -> Patterns -> Tensor -> Tensor
bptEvaluate (Boards bs) (Patterns ps) (Tensor t) = gcTensor $
	withForeignPtr bs \bsRaw ->
	withForeignPtr ps \psRaw ->
	withForeignPtr t \tRaw ->
	C.evaluate bsRaw psRaw tRaw

-- _ -> _ -> p @GPU_FLOAT -> n @Float
-- tUnpackF . evaluate
bptEvaluateSync :: Boards -> Patterns -> Tensor -> Vector Float
bptEvaluateSync b@(Boards bs) (Patterns ps) (Tensor t) = unsafePerformIO $
	withForeignPtr bs \bsRaw ->
	withForeignPtr ps \psRaw ->
	withForeignPtr t \tRaw ->
	allocaArray n \outRaw -> do
	evaluate_sync bsRaw psRaw tRaw outRaw nRaw
	V.generateM n (peekElemOff outRaw)
	where
	nRaw = bSize b
	n = fromIntegral nRaw :: Int

---------- utilities ----------

newWrapper :: (ForeignPtr a -> a) -> FinalizerPtr a -> IO (Ptr a) -> a
newWrapper cons delete ptr = unsafePerformIO (newWrapperIO cons delete =<< ptr)

newWrapperIO :: (ForeignPtr a -> a) -> FinalizerPtr a -> Ptr a -> IO a
newWrapperIO cons delete ptr = cons <$> newForeignPtr delete ptr

{-# Specialize colorIndex :: Color -> Int64 #-}
{-# Specialize colorIndex :: Color -> Word8 #-}
colorIndex :: Num a => Color -> a
colorIndex = \case
	Blue -> 0
	Red -> 1
	Yellow -> 2

{-# Specialize shapeIndex :: Shape -> Int64 #-}
{-# Specialize shapeIndex :: Shape -> Word8 #-}
shapeIndex :: Num a => Shape -> a
shapeIndex = \case
	Virus -> 0
	Disconnected -> 1
	North -> 1
	South -> 1
	East -> 2
	West -> 3

data WithSentinels a = NonSentinel a | EmptySentinel | OutOfBoundsSentinel deriving (Eq, Ord, Read, Show, Functor)

instance Bounded a => Bounded (WithSentinels a) where
	minBound = NonSentinel minBound
	maxBound = OutOfBoundsSentinel

instance Hashable a => Hashable (WithSentinels a) where
	hashWithSalt s = \case
		NonSentinel a -> s `hashWithSalt` distinguisher0 `hashWithSalt` a
		EmptySentinel -> s `hashWithSalt` distinguisher1
		OutOfBoundsSentinel -> s `hashWithSalt` distinguisher2

withSentinels :: [a] -> [WithSentinels a]
withSentinels as = map NonSentinel as ++ [EmptySentinel, OutOfBoundsSentinel]

allColorsWithSentinels :: [WithSentinels Color]
allColorsWithSentinels = withSentinels [minBound..maxBound]

allShapesWithSentinels :: [WithSentinels Shape]
allShapesWithSentinels = withSentinels [Virus, Disconnected, East, West]

colorSentinelIndex :: WithSentinels Color -> Int64
colorSentinelIndex = \case
	NonSentinel a -> colorIndex a
	EmptySentinel -> 3
	OutOfBoundsSentinel -> 4

shapeSentinelIndex :: WithSentinels Shape -> Int64
shapeSentinelIndex = \case
	NonSentinel a -> shapeIndex a
	EmptySentinel -> 4
	OutOfBoundsSentinel -> 5

-- bit patterns 001001001..., 010010010..., and 100100100...
distinguisher0, distinguisher1, distinguisher2 :: Int
distinguisher0 = fromIntegral $ (maxBound :: Word) `quot` 7
distinguisher1 = distinguisher0 `shiftL` 1
distinguisher2 = distinguisher1 `shiftL` 1
