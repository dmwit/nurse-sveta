{-# Language AllowAmbiguousTypes #-}

module Nurse.Sveta.Torch.Endpoint (
	-- * Endpoints
	Endpoint(..), CEndpoint, gcEndpoint,
	cEndpoint, hsEndpoint,
	unmask, batchSize, batchSize', outermostGameConstant,
	dumpEndpoint,
	-- * Marshalling
	-- ** ToEndpoint
	ToEndpoint(..),
	ToEndpointRecordDescription(..), toEndpointRecord,
	OneHot(..), MapLike(..),
	eqAsFloat, oneHotValue,
	OneHotScalar(..), ZeroDefault(..), NoDefault(..), ContainerEndpoint(..),
	-- ** FromEndpoint
	FromEndpoint(..), fromEndpoint,
	FromEndpointRecordDescription(..), endpointIndexRecord, field,
	-- ** Shared
	Structured(..),
	EndpointKey(..),
	-- * Strided vectors
	StridedVector,
	generate, generateM, fromList,
	(!), the,
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
import Data.Coerce
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Kind
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Traversable
import Dr.Mario.Model
import Foreign
import Foreign.C
import GHC.Stack
import Nurse.Sveta.Torch.CWrapper
import Nurse.Sveta.Util
import System.IO

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MVS
import qualified Data.Vector as V

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

data LeafType = Unit | Positive | Categorical | Probability deriving (Bounded, Enum, Eq, Ord, Read, Show)
newtype CLeafType = CLeafType CInt deriving newtype (Eq, Ord, Show, Storable)

-- See [NOTE: FFI, switch, and linking].
c_type_unit, c_type_positive, c_type_categorical, c_type_probability :: CLeafType
c_type_unit:c_type_positive:c_type_categorical:c_type_probability:_ = CLeafType <$> [0..]

foreign import ccall "dump_leaf_type" c_dump_leaf_type :: CLeafType -> IO ()

cLeafType_ :: LeafType -> CLeafType
cLeafType_ = \case
	Unit -> c_type_unit
	Positive -> c_type_positive
	Categorical -> c_type_categorical
	Probability -> c_type_probability

cLeafType :: LeafType -> IO CLeafType
cLeafType = pure . cLeafType_

hsLeafType_ :: HasCallStack => CLeafType -> LeafType
hsLeafType_ ty
	| ty == c_type_unit = Unit
	| ty == c_type_positive = Positive
	| ty == c_type_categorical = Categorical
	| ty == c_type_probability = Probability
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

ifoldMap :: (Monoid m, Storable a) => ([Int] -> a -> m) -> StridedVector a -> m
ifoldMap f StridedVector { strides = ss0, bounds = bs0, contents = c } = go ss0 bs0 [] 0 where
	go [] [] is j = f (reverse is) (c VS.! j)
	go (s:ss) (b:bs) is j = mconcat [go ss bs (i:is) (j+i*s) | i <- [0..b]]
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

newtype CEndpoint = CEndpoint (ForeignPtr CEndpoint) deriving newtype CWrapper

foreign import ccall "new_endpoint_tensor" c_new_endpoint_tensor :: CInt -> CInt -> Ptr CGameConstant -> Ptr CFloat -> Ptr CChar -> IO (Ptr CEndpoint)
foreign import ccall "new_endpoint_vector" c_new_endpoint_vector :: CGameConstant -> Ptr (Ptr CEndpoint) -> IO (Ptr CEndpoint)
foreign import ccall "new_endpoint_dictionary" c_new_endpoint_dictionary :: IO (Ptr CEndpoint)
foreign import ccall "endpoint_add_child" c_endpoint_add_child :: Ptr CEndpoint -> Ptr CChar -> Ptr CEndpoint -> IO ()
foreign import ccall "endpoint_get_tag" c_endpoint_get_tag :: Ptr CEndpoint -> IO CStructureTag
foreign import ccall "endpoint_read_tensor" c_endpoint_read_tensor :: Ptr CInt -> Ptr CInt -> Ptr (Ptr CGameConstant) -> Ptr (Ptr CFloat) -> Ptr CEndpoint -> IO ()
foreign import ccall "endpoint_read_vector" c_endpoint_read_vector :: Ptr CGameConstant -> Ptr (Ptr (Ptr CEndpoint)) -> Ptr CEndpoint -> IO ()
foreign import ccall "endpoint_read_dictionary" c_endpoint_read_dictionary :: Ptr CInt -> Ptr (Ptr CString) -> Ptr (Ptr (Ptr CEndpoint)) -> Ptr CEndpoint -> IO ()
foreign import ccall "dump_endpoint" c_dump_endpoint :: Ptr CEndpoint -> IO ()
foreign import ccall "free_endpoint_read_tensor_constants" c_free_endpoint_read_tensor_constants :: Ptr CGameConstant -> IO ()
foreign import ccall "free_endpoint_read_vector" c_free_endpoint_read_vector :: CGameConstant -> Ptr (Ptr CEndpoint) -> IO ()
foreign import ccall "free_endpoint_read_dictionary" c_free_endpoint_read_dictionary :: CInt -> Ptr CString -> Ptr (Ptr CEndpoint) -> IO ()
foreign import ccall "&free_endpoint_read_tensor_values" c_free_endpoint_read_tensor_values :: FinalizerPtr CFloat
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
	   	alloca \ppValues -> do
	   	c_endpoint_read_tensor pBatchSize pDimCount ppLens ppValues c_e
	   	batchSize <- fromIntegral <$> peek pBatchSize
	   	dimCount <- fromIntegral <$> peek pDimCount
	   	pLens <- peek ppLens
	   	lens <- map hsGameConstant_ <$> peekArray dimCount pLens
	   	let bs = batchSize:evalGameConstants lens
	   	    lenValues:ss = lenStridesFor bs
	   	c_free_endpoint_read_tensor_constants pLens
	   	pValues <- peek ppValues
	   	fpValues <- newForeignPtr c_free_endpoint_read_tensor_values pValues
	   	pure $ EFullTensor lens StridedVector
	   		{ bounds = bs
	   		, strides = ss
	   		, contents = VS.unsafeFromForeignPtr0 fpValues lenValues
	   		}
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

class Structured a where structure :: Structure

class ToEndpoint a where
	-- TODO: would a free functor over Vector (i.e. (Vector top, top -> a)) be more efficient?
	toEndpoint :: V.Vector a -> Endpoint

class FromEndpoint a where
	endpointIndex :: HasCallStack => Endpoint -> Int -> a

fromEndpoint :: (HasCallStack, FromEndpoint a) => Endpoint -> V.Vector a
fromEndpoint e = V.generate (batchSize' e) (endpointIndex e)

instance ToEndpoint CFloat where
	toEndpoint v = EFullTensor [] (generate [V.length v] \[i] -> v V.! i)

instance FromEndpoint CFloat where
	endpointIndex (EFullTensor _ v) i = the (v ! i)

newtype RealToFracEndpoint a = RealToFracEndpoint { getRealToFracEndpoint :: a }
	deriving newtype (Eq, Ord, Fractional, Num, Real)
instance Real a => ToEndpoint (RealToFracEndpoint a) where
	toEndpoint = toEndpoint . fmap (realToFrac @_ @CFloat)
instance Fractional a => FromEndpoint (RealToFracEndpoint a) where
	endpointIndex e i = realToFrac @CFloat (endpointIndex e i)

deriving via RealToFracEndpoint Float instance ToEndpoint Float
deriving via RealToFracEndpoint Float instance FromEndpoint Float
deriving via RealToFracEndpoint CUChar instance ToEndpoint CUChar

-- | Assumes 8x16 boards
instance Structured Board where
	structure = SDictionary $ tail [undefined
		, ("shape", STensor Unit [GCShapes, GCWidth, GCHeight])
		, ("color", STensor Unit [GCColors, GCWidth, GCHeight])
		]

-- | Assumes that all boards have the same size
instance ToEndpoint Board where
	toEndpoint boards = EDictionary $ tail [undefined
		, ("shape", full shape)
		, ("color", full color)
		] where
		full :: forall a. (Eq a, OneHot a) => (Cell -> Maybe a) -> Endpoint
		full f = EFullTensor gcs $ generate
			(V.length boards:map evalGameConstant gcs)
			-- it is important to use toIndex rather than fromIndex here,
			-- because the Shape instance collapses multiple shapes to the same
			-- index
			\[i, v, x, y] -> eqAsFloat (Just v)
				(toIndex <$> f (unsafeGet (boards V.! i) (Position x y)))
			where gcs = [indexCountGC @a, gcw, gch]
		(gcw, gch) = case boards V.!? 0 of
			Nothing -> (GCWidth, GCHeight)
			Just b -> (GCWidth `orMiscellaneous` width b, GCHeight `orMiscellaneous` height b)

infix 3 :=:
infixr 2 :&:
-- | Together with 'toEndpointRecord', this makes defining 'ToEndpoint' instances for
-- record types relatively easy. For example, for
--
-- > data Foo = { bar :: Bar, baz :: Baz }
--
-- you might write the instance
--
-- > instance ToEndpoint Foo where toEndpoint = toEndpointRecord $ "bar" :=: bar :&: "baz" :=: baz
--
-- This requires @Bar@ and @Baz@ to have their own 'ToEndpoint' instances.
data ToEndpointRecordDescription a where
	(:=:) :: ToEndpoint tgt => String -> (src -> tgt) -> ToEndpointRecordDescription src
	(:&:) :: ToEndpointRecordDescription a -> ToEndpointRecordDescription a -> ToEndpointRecordDescription a

-- | See 'ToEndpointDescription'.
toEndpointRecord :: HasCallStack => ToEndpointRecordDescription a -> V.Vector a -> Endpoint
toEndpointRecord desc vs = EDictionary (go desc) where
	go (nm :=: f) = [(nm, toEndpoint (f <$> vs))]
	go (l :&: r) = go l ++ go r

class OneHot a where
	indexCount :: Int
	indexCountGC :: GameConstant
	toIndex :: a -> Int
	fromIndex :: Int -> a

	indexCount = evalGameConstant (indexCountGC @a)
	indexCountGC = GCMiscellaneous (indexCount @a)

	default toIndex :: Enum a => a -> Int
	default fromIndex :: Enum a => Int -> a
	toIndex = fromEnum
	fromIndex = toEnum

instance OneHot Color where indexCountGC = GCColors
instance OneHot Orientation where indexCountGC = GCOrientations

instance OneHot Shape where
	indexCountGC = GCShapes
	toIndex = \case
		Virus -> 0
		West -> 1
		East -> 2
		_ -> 3 -- Disconnected, North, and South all have the same strategic content
	fromIndex = \case
		0 -> Virus
		1 -> West
		2 -> East
		3 -> Disconnected

instance OneHot PillContent where
	indexCount = indexCount @Orientation * indexCount @Color * indexCount @Color
	toIndex pc = (toIndex (orientation pc) * indexCount @Color + toIndex (bottomLeftColor pc)) * indexCount @Color + toIndex (otherColor pc)
	fromIndex n = PillContent
		{ orientation = fromIndex o
		, bottomLeftColor = fromIndex l
		, otherColor = fromIndex r
		} where
		(n', r) = n `quotRem` indexCount @Color
		(o , l) = n' `quotRem` indexCount @Color

eqAsFloat :: Eq a => a -> a -> CFloat
eqAsFloat x y = if x == y then 1 else 0

-- | Handy for building an argument to 'generate'; takes an actual value and
-- the one-hot index we're currently generating the value for.
oneHotValue :: (Eq a, OneHot a) => a -> Int -> CFloat
oneHotValue = eqAsFloat . toIndex

-- | A newtype to hang some Endpoint instances off of. See also 'SingletonSet',
-- which is essentially the same, but supports types that would prefer to be
-- represented as a collection of indices rather than a single index.
newtype OneHotScalar a = OneHotScalar { getOneHotScalar :: a }
	deriving (Eq, Ord, Read, Show)

instance (Eq a, OneHot a) => ToEndpoint (OneHotScalar a) where
	toEndpoint vs = EFullTensor [indexCountGC @a]
		$ generate [V.length vs, indexCount @a] \[n, i] -> oneHotValue (getOneHotScalar (vs V.! n)) i

-- | @toIndices . fromIndices = id@ is a law, but @fromIndices . toIndices =
-- id@ is not (e.g. see the 'Shape' instance, where 'North', 'South', and
-- 'Disconnected' all have the same index).
class Eq a => EndpointKey a where
	indexCounts :: [GameConstant]
	toIndices :: a -> [Int]
	fromIndices :: HasCallStack => [Int] -> a

	default indexCounts :: OneHot a => [GameConstant]
	indexCounts = [indexCountGC @a]

	default toIndices :: OneHot a => a -> [Int]
	toIndices = (:[]) . toIndex

	default fromIndices :: (HasCallStack, OneHot a) => [Int] -> a
	fromIndices [i] = fromIndex i
	fromIndices is = error $ "Expected one index to decode, but saw " ++ show (length is) ++ ", namely, " ++ show is

instance EndpointKey Orientation
instance EndpointKey Color
instance EndpointKey Shape

instance EndpointKey Lookahead where
	indexCounts = [indexCountGC @Color, indexCountGC @Color]
	toIndices lk = [toIndex (f lk) | f <- [leftColor, rightColor]]
	fromIndices [l, r] = Lookahead
		{ leftColor = fromIndex l
		, rightColor = fromIndex r
		}

instance EndpointKey PillContent where
	indexCounts = [indexCountGC @Orientation, indexCountGC @Color, indexCountGC @Color]
	toIndices pc = [toIndex (orientation pc), toIndex (bottomLeftColor pc), toIndex (otherColor pc)]
	fromIndices [o, bl, oc] = PillContent
		{ orientation = fromIndex o
		, bottomLeftColor = fromIndex bl
		, otherColor = fromIndex oc
		}

-- | BEWARE! This instance assumes that positions are in-bounds for a standard 8x16 board.
instance EndpointKey Position where
	indexCounts = [GCWidth, GCHeight]
	toIndices pos = [x pos, y pos]
	fromIndices [ix, iy] = Position { x = ix, y = iy }

-- | BEWARE! This instance assumes that pills are being placed on a standard 8x16 board.
instance EndpointKey Pill where
	indexCounts = indexCounts @PillContent ++ indexCounts @Position
	toIndices pill = toIndices (content pill) ++ toIndices (bottomLeftPosition pill)
	fromIndices (splitAt (length (indexCounts @PillContent)) -> (pc, pos)) = Pill
		{ content = fromIndices pc
		, bottomLeftPosition = fromIndices pos
		}

class (forall k v. KeyLike m k => Monoid (m k v)) => MapLike m where
	type KeyLike m :: Type -> Constraint
	mapToList :: m k v -> [(k, v)]
	mapSingleton :: KeyLike m k => k -> v -> m k v

instance MapLike HashMap where
	type KeyLike HashMap = Hashable
	mapToList = HM.toList
	mapSingleton = HM.singleton

instance MapLike Map where
	type KeyLike Map = Ord
	mapToList = M.toList
	mapSingleton = M.singleton

-- | Some map-like types represent partial mappings, but some represent total
-- mappings with some default. If your mapping does not have a reasonable
-- default, you can use this newtype to get an appropriately-masked
-- 'ToEndpoint' instance.
newtype NoDefault m k v = NoDefault { getNoDefault :: m k v }

instance (MapLike m, EndpointKey k, Real v) => ToEndpoint (NoDefault m k v) where
	toEndpoint ms = EMaskedTensor gcs
		(fromList is (assocs realToFrac))
		(fromList is (assocs (const 1)))
		where
		gcs = indexCounts @k
		is = V.length ms : map evalGameConstant gcs

		assocs :: (v -> a) -> [([Int], a)]
		assocs f = V.ifoldr (\i (NoDefault m) writes -> [(i:toIndices k, f v) | (k, v) <- mapToList m] ++ writes) [] ms

-- | Some map-like types really represent partial mappings, but some represent
-- total mappings with some default. If your mapping has a default of zero, you
-- can use this newtype to get an appropriate unmasked 'ToEndpoint' instance.
newtype ZeroDefault m k v = ZeroDefault { getZeroDefault :: m k v }

instance (MapLike m, EndpointKey k, Real v) => ToEndpoint (ZeroDefault m k v) where
	toEndpoint ms = unmask (toEndpoint (coerce ms :: V.Vector (NoDefault m k v)))

deriving via NoDefault HashMap k v instance (EndpointKey k, Real v) => ToEndpoint (HashMap k v)
deriving via NoDefault Map     k v instance (EndpointKey k, Real v) => ToEndpoint (Map     k v)

-- | If you have a set-like type, you can use this newtype to get a
-- 'ToEndpoint' instance that has a full tensor with ones at indices
-- corresponding to members of the set and zeros elsewhere.
newtype ContainerEndpoint f a = ContainerEndpoint { getContainerEndpoint :: f a }

instance (Foldable f, EndpointKey a) => ToEndpoint (ContainerEndpoint f a) where
	toEndpoint sets = EFullTensor gcs $ fromList
		(V.length sets : map evalGameConstant gcs)
		(V.ifoldr (\i (ContainerEndpoint set) writes -> foldr (\v writes' -> (i:toIndices v, 1):writes') writes set) [] sets)
		where
		gcs = indexCounts @a

deriving via ContainerEndpoint HashSet a instance EndpointKey a => ToEndpoint (HashSet a)

-- TODO: could support EVector maybe?
endpointIndexMapLike :: (HasCallStack, MapLike m, KeyLike m k, EndpointKey k, Fractional v) => Endpoint -> Int -> m k v
endpointIndexMapLike (EFullTensor _ t) i = ifoldMap (\is v -> mapSingleton (fromIndices is) (realToFrac v)) (t ! i)

instance (EndpointKey k, Fractional v, Hashable k) => FromEndpoint (HashMap k v) where endpointIndex = endpointIndexMapLike
instance (EndpointKey k, Fractional v, Ord      k) => FromEndpoint (    Map k v) where endpointIndex = endpointIndexMapLike

newtype FromEndpointRecordDescription a = FromEndpointRecordDescription
	{ getFromEndpointRecordDescription :: HasCallStack => HashMap String Endpoint -> Int -> a }
	deriving (Functor)

instance Applicative FromEndpointRecordDescription where
	pure v = FromEndpointRecordDescription \_ _ -> v
	FromEndpointRecordDescription f <*> FromEndpointRecordDescription v
		= FromEndpointRecordDescription \es i -> f es i (v es i)

instance Monad FromEndpointRecordDescription where
	FromEndpointRecordDescription v >>= f = FromEndpointRecordDescription \es i ->
		getFromEndpointRecordDescription (f (v es i)) es i

field :: (HasCallStack, FromEndpoint a) => String -> FromEndpointRecordDescription a
field s = FromEndpointRecordDescription \es -> case HM.lookup s es of
	Just e -> endpointIndex e
	Nothing -> error $ "missing field " ++ show s ++ " while trying to decode endpoint; fields available include " ++ show (HM.keys es)

endpointIndexRecord :: HasCallStack => FromEndpointRecordDescription a -> Endpoint -> Int -> a
endpointIndexRecord (FromEndpointRecordDescription f) = \case
	EDictionary dict -> f (HM.fromList dict)
	e -> error $ "attempted to decode a non-dictionary endpoint as if it were record-like\nfull endpoint: " ++ show e
