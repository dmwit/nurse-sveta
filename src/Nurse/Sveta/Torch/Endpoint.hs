module Nurse.Sveta.Torch.Endpoint (
	-- * Endpoints
	Endpoint(..), CEndpoint, gcEndpoint,
	cEndpoint, hsEndpoint,
	unmask, batchSize, batchSize', outermostGameConstant,
	dumpEndpoint,
	-- * Strided vectors
	StridedVector,
	generate, generateM, fromList,
	(!), the, svMap, svReplicate, ifoldMap,
	-- * Structures
	Structure(..), CStructure, gcStructure,
	cStructure,
	dumpStructure,
	-- * Leaf types
	LeafType(..), CLeafType,
	cLeafType, cLeafType_, hsLeafType, hsLeafType_,
	-- * Sizes
	GameConstant(..), CGameConstant,
	cGameConstant_, cGameConstant, hsGameConstant_, hsGameConstant,
	orMiscellaneous,
	dumpGameConstant,
	evalGameConstant, evalGameConstants, evalCGameConstant,
	) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Foreign
import Foreign.C
import GHC.Stack
import Nurse.Sveta.Torch.CWrapper
import Nurse.Sveta.Util
import System.IO
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MVS

data GameConstant = GCMiscellaneous Int | GCColors | GCShapes | GCWidth | GCHeight | GCOrientations deriving (Eq, Ord, Read, Show)
newtype CGameConstant = CGameConstant CInt deriving newtype (Eq, Ord, Show, Storable)

-- [NOTE: FFI, switch, and linking]
--
-- I'm in a bit of a bind here. What I want to do is declare
--     extern const int c_colors;
-- in endpoint.h, put
--     const int c_colors = 0;
-- in endpoint.cpp, and declare
--     foreign import capi "endpoint.h value c_colors" c_c_colors :: CGameConstant
-- here. All of that works, but if I do that, then I can't use c_colors in a
-- switch/case in adapter.cpp. I could if I declared it to be a constexpr, but
-- that's not C compatible so there isn't a good way to include that in
-- endpoint.h, and you can't declare constexpr values, only define them. Or I
-- could put
--     const int c_colors = 0;
-- straight into endpoint.h, but then there's a linking problem with retrieving
-- that constant from Haskell, since each C file gets its own copy.
--
-- What I ended up with is to put a #define in endpoint.h, which eliminates
-- linking questions and allows switch/case. From my reading of the capi
-- documentation, it *ought* to also enable the foreign import described above.
-- But when I try, I again get linking errors. Perhaps this is a bug in GHC, or
-- in its documentation?
c_c_colors, c_c_shapes, c_c_width, c_c_height, c_c_orientations :: CGameConstant
c_c_colors:c_c_shapes:c_c_width:c_c_height:c_c_orientations:_ = CGameConstant <$> [0..]

foreign import ccall "eval_game_constant" c_eval_game_constant :: CGameConstant -> CInt
foreign import ccall "dump_game_constant" c_dump_game_constant :: CGameConstant -> IO ()

cGameConstant_ :: GameConstant -> CGameConstant
cGameConstant_ = \case
	GCMiscellaneous n -> CGameConstant . complement . fromIntegral $ n
	GCColors -> c_c_colors
	GCShapes -> c_c_shapes
	GCWidth -> c_c_width
	GCHeight -> c_c_height
	GCOrientations -> c_c_orientations

cGameConstant :: GameConstant -> IO CGameConstant
cGameConstant = pure . cGameConstant_

hsGameConstant_ :: HasCallStack => CGameConstant -> GameConstant
hsGameConstant_ tag@(CGameConstant n)
	| tag == c_c_colors = GCColors
	| tag == c_c_shapes = GCShapes
	| tag == c_c_width = GCWidth
	| tag == c_c_height = GCHeight
	| tag == c_c_orientations = GCOrientations
	| n < 0 = GCMiscellaneous . fromIntegral . complement $ n
	| otherwise = error $ "Unknown game constant tag " ++ show tag

hsGameConstant :: HasCallStack => CGameConstant -> IO GameConstant
hsGameConstant = evaluate . hsGameConstant_

evalGameConstant :: GameConstant -> Int
evalGameConstant = \case
	GCMiscellaneous n -> n
	GCColors -> 3
	GCShapes -> 4
	GCWidth -> 8
	GCHeight -> 16
	GCOrientations -> 2

evalGameConstants :: Functor f => f GameConstant -> f Int
evalGameConstants = fmap evalGameConstant

evalCGameConstant :: CGameConstant -> Int
evalCGameConstant = fromIntegral . c_eval_game_constant

dumpGameConstant :: CGameConstant -> IO ()
dumpGameConstant c = hFlush stdout >> c_dump_game_constant c

orMiscellaneous :: GameConstant -> Int -> GameConstant
orMiscellaneous gc n
	| evalGameConstant gc == n = gc
	| otherwise = GCMiscellaneous n

data LeafType = Unit | Positive | Categorical | Probability | Unbounded deriving (Bounded, Enum, Eq, Ord, Read, Show)
newtype CLeafType = CLeafType CInt deriving newtype (Eq, Ord, Show, Storable)

-- See [NOTE: FFI, switch, and linking].
c_type_unit, c_type_positive, c_type_categorical, c_type_probability :: CLeafType
c_type_unit:c_type_positive:c_type_categorical:c_type_probability:c_type_unbounded:_ = CLeafType <$> [0..]

foreign import ccall "dump_leaf_type" c_dump_leaf_type :: CLeafType -> IO ()

cLeafType_ :: LeafType -> CLeafType
cLeafType_ = \case
	Unit -> c_type_unit
	Positive -> c_type_positive
	Categorical -> c_type_categorical
	Probability -> c_type_probability
	Unbounded -> c_type_unbounded

cLeafType :: LeafType -> IO CLeafType
cLeafType = pure . cLeafType_

hsLeafType_ :: HasCallStack => CLeafType -> LeafType
hsLeafType_ ty
	| ty == c_type_unit = Unit
	| ty == c_type_positive = Positive
	| ty == c_type_categorical = Categorical
	| ty == c_type_probability = Probability
	| ty == c_type_unbounded = Unbounded
	| otherwise = error $ "Unknown leaf type " ++ show ty

hsLeafType :: HasCallStack => CLeafType -> IO LeafType
hsLeafType = evaluate . hsLeafType_

dumpLeafType :: CLeafType -> IO ()
dumpLeafType c = hFlush stdout >> c_dump_leaf_type c

newtype CStructureTag = CStructureTag CInt deriving newtype (Eq, Ord, Show, Storable)

-- See [NOTE: FFI, switch, and linking].
c_tag_tensor, c_tag_vector, c_tag_dictionary :: CStructureTag
c_tag_tensor:c_tag_vector:c_tag_dictionary:_ = CStructureTag <$> [0..]

data Structure
	= STensor LeafType [GameConstant]
	| SVector GameConstant Structure
	| SDictionary [(String, Structure)]
	deriving (Eq, Ord, Read, Show)

newtype CStructure = CStructure (ForeignPtr CStructure) deriving newtype CWrapper

foreign import ccall "new_structure_tensor" c_new_structure_tensor :: CLeafType -> CInt -> Ptr CGameConstant -> IO (Ptr CStructure)
foreign import ccall "new_structure_vector" c_new_structure_vector :: CGameConstant -> Ptr CStructure -> IO (Ptr CStructure)
foreign import ccall "new_structure_dictionary" c_new_structure_dictionary :: IO (Ptr CStructure)
foreign import ccall "structure_add_child" c_structure_add_child :: Ptr CStructure -> Ptr CChar -> Ptr CStructure -> IO ()
foreign import ccall "dump_structure" c_dump_structure :: Ptr CStructure -> IO ()
foreign import ccall "&free_structure" c_free_structure :: FinalizerPtr CStructure

gcStructure :: Ptr CStructure -> IO CStructure
gcStructure = fmap CStructure . newForeignPtr c_free_structure

cStructure :: Structure -> IO CStructure
cStructure = (gcStructure =<<) . \case
	STensor ty cs -> withArrayLen (map cGameConstant_ cs) (c_new_structure_tensor (cLeafType_ ty) . fromIntegral)
	SVector c s -> do
		c_s <- cStructure s
		withUnwrapped c_s $ c_new_structure_vector (cGameConstant_ c)
	SDictionary dict -> do
		parent <- c_new_structure_dictionary
		for_ dict \(nm, child) -> do
			fpChild <- cStructure child
			withUnwrapped (WCS nm, fpChild) \(c_nm, c_child) -> c_structure_add_child parent c_nm c_child
		pure parent

dumpStructure :: CStructure -> IO ()
dumpStructure s = hFlush stdout >> withUnwrapped s c_dump_structure

data StridedVector a = StridedVector
	{ strides :: [Int]
	, bounds :: [Int]
	, contents :: VS.Vector a
	-- TODO: these instances are not semantic (for Read/Show, do we even want them to be?)
	} deriving (Eq, Ord, Read, Show)

lenStridesFor :: [Int] -> [Int]
lenStridesFor = scanr (*) 1

stridesFor :: [Int] -> [Int]
stridesFor = tail . lenStridesFor

indicesFor :: HasCallStack => [Int] -> Int -> [Int]
indicesFor (s:ss) i = q : indicesFor ss r where (q, r) = i `quotRem` s
indicesFor [] 0 = []
indicesFor [] n = error $ "Index was not a multiple of the smallest stride; leftover bit was " ++ show n

indexFor :: HasCallStack => [Int] -> [Int] -> Int
indexFor (s:ss) (i:is) = s*i + indexFor ss is
indexFor [] [] = 0
indexFor ss [] = error $ "More strides than indices in indicesFor; leftover bit was " ++ show ss
indexFor [] is = error $ "More indices than strides in indicesFor; leftover bit was " ++ show is

generate :: Storable a => [Int] -> ([Int] -> a) -> StridedVector a
generate bs f = StridedVector
	{ strides = ss
	, bounds = bs
	, contents = VS.generate len (f . indicesFor ss)
	} where
	len:ss = lenStridesFor bs

generateM :: (Storable a, Monad m) => [Int] -> ([Int] -> m a) -> m (StridedVector a)
generateM bs f = StridedVector ss bs <$> VS.generateM len (f . indicesFor ss) where
	len:ss = lenStridesFor bs

-- | Indices not listed in the second argument will be initialized to all-zeros.
fromList :: Storable a => [Int] -> [([Int], a)] -> StridedVector a
fromList bs writes = StridedVector
	{ strides = ss
	, bounds = bs
	, contents = VS.create do
		vec <- MVS.new len
		for_ writes \(is, a) -> MVS.write vec (indexFor ss is) a
		pure vec
	} where
	len:ss = lenStridesFor bs

the :: (HasCallStack, Storable a) => StridedVector a -> a
the StridedVector { bounds = bs, contents = c } = case bs of
	[] -> c VS.! 0
	_ -> error $ "Tried to retrieve the unique element from a strided vector with non-empty bounds " ++ show bs ++ ".\nThis is almost always a bug, even if those bounds really do result in just one element."

infixl 9 !
(!) :: (HasCallStack, Storable a) => StridedVector a -> Int -> StridedVector a
StridedVector { strides = s:ss, bounds = b:bs, contents = c } ! i | 0 <= i && i < b = StridedVector
	{ strides = ss
	, bounds = bs
	, contents = VS.drop (i*s) c
	}
sv ! i = error $ "Indexing error while computing StridedVector { strides = " ++ show (strides sv) ++ ", bounds = " ++ show (bounds sv) ++ ", contents = <elided> } ! " ++ show i

svMap :: (Storable a, Storable b) => (a -> b) -> StridedVector a -> StridedVector b
svMap f sv = sv { contents = VS.map f (contents sv) }

svReplicate :: Int -> StridedVector a -> StridedVector a
svReplicate n sv = sv
	{ strides = 0:strides sv
	, bounds = n:bounds sv
	}

ifoldMap :: (Monoid m, Storable a) => ([Int] -> a -> m) -> StridedVector a -> m
ifoldMap f StridedVector { strides = ss0, bounds = bs0, contents = c } = go ss0 bs0 [] 0 where
	go [] [] is j = f (reverse is) (c VS.! j)
	go (s:ss) (b:bs) is j = mconcat [go ss bs (i:is) (j+i*s) | i <- [0..b-1]]
	go _ _ _ _ = error $ "Internal error in ifoldMap: strides and bounds did not have the same length"

data Endpoint
	= EFullTensor [GameConstant] (StridedVector CFloat)
	-- Currently, the ZeroDefault instances rely on the mask field being lazy.
	-- If this field becomes strict, revisit those -- probably we will need to
	-- invert the call structure, so that the ZeroDefault instance is basic,
	-- and the masked versions of the maps call it. You might want to audit the
	-- codebase for other calls to unmask, too.
	| EMaskedTensor [GameConstant] (StridedVector CFloat) (StridedVector CChar)
	| EVector GameConstant [Endpoint]
	| EDictionary [(String, Endpoint)]
	deriving (Eq, Ord, Read, Show)

unmask :: Endpoint -> Endpoint
unmask = \case
	v@(EFullTensor{}) -> v
	EMaskedTensor cs vals mask -> EFullTensor cs vals
	EVector c es -> EVector c (unmask <$> es)
	EDictionary dict -> EDictionary (fmap (fmap unmask) dict)

batchSize :: Endpoint -> Maybe Int
batchSize = \case
	EFullTensor _ (StridedVector { bounds = n:_ }) -> Just n
	EMaskedTensor _ (StridedVector { bounds = n:_ }) _ -> Just n
	EVector _ es -> asum (batchSize <$> es)
	EDictionary dict -> asum (batchSize . snd <$> dict)

-- | If the batch size is indeterminate, this returns 0.
batchSize' :: Endpoint -> Int
batchSize' = fromMaybe 0 . batchSize

outermostGameConstant :: Endpoint -> Maybe GameConstant
outermostGameConstant = \case
	EFullTensor (gc:_) _ -> Just gc
	EMaskedTensor (gc:_) _ _ -> Just gc
	EVector gc _ -> Just gc
	_ -> Nothing

data EndpointShape = ShFull [GameConstant] | ShMasked [GameConstant] | ShVector GameConstant EndpointShape | ShDictionary [(String, EndpointShape)]
	deriving (Eq, Ord, Read, Show)

sanityCheck :: Endpoint -> Either String (Int, EndpointShape)
sanityCheck = \case
	EFullTensor gcs vals -> case bounds vals of
		len:rest | rest == evalGameConstants gcs -> Right (len, ShFull gcs)
		         | otherwise -> Left $ "StridedVector dimensions don't match tensor dimensions (" ++ show (bounds vals) ++ " vs. " ++ show gcs ++ ")"
		[] -> Left "StridedVector has no batch dimension"
	EMaskedTensor gcs vals mask
		| bounds vals /= bounds mask -> Left $ "Value dimensions don't match mask dimensions (" ++ show (bounds vals) ++ " vs. " ++ show (bounds mask) ++ ")"
		| otherwise -> case bounds vals of
			len:rest | rest == evalGameConstants gcs -> Right (len, ShMasked gcs)
			         | otherwise -> Left $ "StridedVector dimensions don't match tensor dimensions (" ++ show (bounds vals) ++ " vs. " ++ show gcs ++ ")"
			[] -> Left "StridedVector has no batch dimension"
	EVector gc es | evalGameConstant gc /= length es -> Left $ "Vector dimension doesn't match content length (" ++ show gc ++ " vs. " ++ show (length es) ++ ")"
	              | otherwise -> case namedTraverse sanityCheck (zip [0..] es) of
	              	Right (lenty@(len, ty):lentys) | all (lenty==) lentys -> Right (len, ShVector gc ty)
	              	                               | otherwise -> Left $ "Heterogeneous vector: " ++ show (lenty:lentys)
	              	Right [] -> Left $ "Indeterminate batch size due to empty vector"
	              	Left (i, err) -> Left $ "[" ++ show i ++ "]=>" ++ err
	EDictionary dict -> case namedTraverse sanityCheck dict of
		Right lentys@((len, _):_) | all ((len==).fst) lentys -> Right (len, ShDictionary (zipWith (\(nm, _) (_, ty) -> (nm, ty)) dict lentys))
		                          | otherwise -> Left $ "Mixed batch sizes in dictionary: " ++ show (zipWith (\(nm, _) (len, _) -> (nm, len)) dict lentys)
		Right [] -> Left $ "Indeterminate batch size due to empty dictionary"
		Left (nm, err) -> Left $ "{" ++ nm ++ "}=>" ++ err

namedTraverse :: (a -> Either e b) -> [(nm, a)] -> Either (nm, e) [b]
namedTraverse f = go where
	go = \case
		[] -> pure []
		(nm, a):as -> case f a of
			Left e -> Left (nm, e)
			Right b -> (b:) <$> go as

newtype CEndpoint = CEndpoint (ForeignPtr CEndpoint) deriving newtype CWrapper

foreign import ccall "new_endpoint_tensor" c_new_endpoint_tensor :: CInt -> CInt -> Ptr CGameConstant -> Ptr CFloat -> Ptr CChar -> IO (Ptr CEndpoint)
foreign import ccall "new_endpoint_vector" c_new_endpoint_vector :: CGameConstant -> Ptr (Ptr CEndpoint) -> IO (Ptr CEndpoint)
foreign import ccall "new_endpoint_dictionary" c_new_endpoint_dictionary :: IO (Ptr CEndpoint)
foreign import ccall "endpoint_add_child" c_endpoint_add_child :: Ptr CEndpoint -> Ptr CChar -> Ptr CEndpoint -> IO ()
foreign import ccall "endpoint_get_tag" c_endpoint_get_tag :: Ptr CEndpoint -> IO CStructureTag
foreign import ccall "endpoint_read_tensor" c_endpoint_read_tensor :: Ptr CInt -> Ptr CInt -> Ptr (Ptr CGameConstant) -> Ptr (Ptr CFloat) -> Ptr (Ptr CChar) -> Ptr CEndpoint -> IO ()
foreign import ccall "endpoint_read_vector" c_endpoint_read_vector :: Ptr CGameConstant -> Ptr (Ptr (Ptr CEndpoint)) -> Ptr CEndpoint -> IO ()
foreign import ccall "endpoint_read_dictionary" c_endpoint_read_dictionary :: Ptr CInt -> Ptr (Ptr CString) -> Ptr (Ptr (Ptr CEndpoint)) -> Ptr CEndpoint -> IO ()
foreign import ccall "dump_endpoint" c_dump_endpoint :: Ptr CEndpoint -> IO ()
foreign import ccall "free_endpoint_read_tensor_constants" c_free_endpoint_read_tensor_constants :: Ptr CGameConstant -> IO ()
foreign import ccall "free_endpoint_read_vector" c_free_endpoint_read_vector :: CGameConstant -> Ptr (Ptr CEndpoint) -> IO ()
foreign import ccall "free_endpoint_read_dictionary" c_free_endpoint_read_dictionary :: CInt -> Ptr CString -> Ptr (Ptr CEndpoint) -> IO ()
foreign import ccall "&free_endpoint_read_tensor_values" c_free_endpoint_read_tensor_values :: FinalizerPtr CFloat
foreign import ccall "&free_endpoint_read_tensor_mask" c_free_endpoint_read_tensor_mask :: FinalizerPtr CChar
foreign import ccall "&free_endpoint" c_free_endpoint :: FinalizerPtr CEndpoint

gcEndpoint :: Ptr CEndpoint -> IO CEndpoint
gcEndpoint = fmap CEndpoint . newForeignPtr c_free_endpoint

endpointGetTag :: CEndpoint -> IO CStructureTag
endpointGetTag e = withUnwrapped e c_endpoint_get_tag

cEndpoint :: HasCallStack => Endpoint -> IO CEndpoint
cEndpoint = (gcEndpoint =<<) . \case
	EFullTensor cs StridedVector { bounds = b:bs, contents = vals }
		| bs == evalGameConstants cs ->
			withArrayLen (map cGameConstant_ cs) \n c_cs ->
			VS.unsafeWith vals \c_vals ->
			c_new_endpoint_tensor (fromIntegral b) (fromIntegral n) c_cs c_vals nullPtr
	EMaskedTensor cs StridedVector { bounds = b:bs, contents = vals } StridedVector { bounds = bs', contents = mask }
		| bs == evalGameConstants cs && b:bs == bs' ->
			withArrayLen (map cGameConstant_ cs) \n c_cs ->
			VS.unsafeWith vals \c_vals ->
			VS.unsafeWith mask \c_mask ->
			c_new_endpoint_tensor (fromIntegral b) (fromIntegral n) c_cs c_vals c_mask
	EVector c es
		| evalGameConstant c == length es ->
			traverse cEndpoint es >>= \es' ->
			withUnwrapped es' \c_es ->
			withArray c_es \c_es' ->
			c_new_endpoint_vector (cGameConstant_ c) c_es'
	EDictionary dict -> do
		parent <- c_new_endpoint_dictionary
		for_ dict \(nm, child) -> do
			fpChild <- cEndpoint child
			withUnwrapped (WCS nm, fpChild) \(c_nm, c_child) ->
				c_endpoint_add_child parent c_nm c_child
		pure parent
	e -> error $ "Unsupported endpoint in cEndpoint: " ++ show e

hsEndpoint :: HasCallStack => CEndpoint -> IO Endpoint
hsEndpoint e = withUnwrapped e hsEndpoint_

hsEndpoint_ :: HasCallStack => Ptr CEndpoint -> IO Endpoint
hsEndpoint_ c_e = do
	tag <- c_endpoint_get_tag c_e
	if | tag == c_tag_tensor ->
	   	alloca \pBatchSize ->
	   	alloca \pDimCount ->
	   	alloca \ppLens ->
	   	alloca \ppValues ->
	   	alloca \ppMask -> do
	   	c_endpoint_read_tensor pBatchSize pDimCount ppLens ppValues ppMask c_e
	   	batchSize <- fromIntegral <$> peek pBatchSize
	   	dimCount <- fromIntegral <$> peek pDimCount
	   	pLens <- peek ppLens
	   	lens <- map hsGameConstant_ <$> peekArray dimCount pLens
	   	let bs = batchSize:evalGameConstants lens
	   	    lenValues:ss = lenStridesFor bs
	   	c_free_endpoint_read_tensor_constants pLens
	   	pValues <- peek ppValues
	   	pMask <- peek ppMask
	   	fpValues <- newForeignPtr c_free_endpoint_read_tensor_values pValues
	   	fpMask <- newForeignPtr c_free_endpoint_read_tensor_mask pMask
	   	let svValues = StridedVector
	   	    	{ bounds = bs
	   	    	, strides = ss
	   	    	, contents = VS.unsafeFromForeignPtr0 fpValues lenValues
	   	    	}
	   	pure $ if pMask == nullPtr
	   		then EFullTensor lens svValues
	   		else EMaskedTensor lens svValues svValues
	   			{ contents = VS.unsafeFromForeignPtr0 fpMask lenValues }
	   | tag == c_tag_vector ->
	   	alloca \pLen ->
	   	alloca \ppChildren -> do
	   	c_endpoint_read_vector pLen ppChildren c_e
	   	c_len <- peek pLen
	   	let len = hsGameConstant_ c_len
	   	pChildren <- peek ppChildren
	   	c_children <- peekArray (evalGameConstant len) pChildren
	   	children <- traverse hsEndpoint_ c_children
	   	c_free_endpoint_read_vector c_len pChildren
	   	pure $ EVector len children
	   | tag == c_tag_dictionary ->
	   	alloca \pCount ->
	   	alloca \ppNames ->
	   	alloca \ppChildren -> do
	   	c_endpoint_read_dictionary pCount ppNames ppChildren c_e
	   	c_count <- peek pCount
	   	let count = fromIntegral c_count
	   	pNames <- peek ppNames
	   	c_names <- peekArray count pNames
	   	names <- traverse peekCString c_names
	   	pChildren <- peek ppChildren
	   	c_children <- peekArray count pChildren
	   	children <- traverse hsEndpoint_ c_children
	   	c_free_endpoint_read_dictionary c_count pNames pChildren
	   	pure $ EDictionary (zip names children)
	   | otherwise -> error $ "Unknown structure tag " ++ show tag ++ " in hsEndpoint"

dumpEndpoint :: CEndpoint -> IO ()
dumpEndpoint e = hFlush stdout >> withUnwrapped e c_dump_endpoint
