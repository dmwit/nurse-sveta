module Nurse.Sveta.Torch.Endpoint (
	-- * Endpoints
	Endpoint(..), MaskedEndpoint(..), CEndpoint,
	cEndpoint, cMaskedEndpoint, hsEndpoint,
	unmask,
	dumpEndpoint,
	-- * Strided vectors
	StridedVector,
	generate, generateM,
	(!), the,
	-- * Structures
	Structure(..), CStructure,
	cStructure,
	dumpStructure,
	-- * Leaf types
	LeafType(..),
	-- * Dimensions
	Dimensions(..), GameConstant(..),
	cDimensions, hsDimensions, cGameConstant_, cGameConstant, hsGameConstant_, hsGameConstant,
	evalDimensions, evalGameConstant, evalCGameConstant,
	dumpDimensions, dumpGameConstant,
	) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.Vector.Storable (Vector)
import Foreign
import Foreign.C
import GHC.Stack
import System.IO

import qualified Data.Vector.Storable as V

import Nurse.Sveta.Util

data GameConstant = GCColors | GCShapes | GCWidth | GCHeight | GCOrientations deriving (Bounded, Enum, Eq, Ord, Read, Show)
newtype CGameConstant = CGameConstant CInt deriving newtype (Eq, Ord, Show, Storable)

foreign import capi "endpoint.h value c_colors" c_c_colors :: CGameConstant
foreign import capi "endpoint.h value c_shapes" c_c_shapes :: CGameConstant
foreign import capi "endpoint.h value c_width" c_c_width :: CGameConstant
foreign import capi "endpoint.h value c_height" c_c_height :: CGameConstant
foreign import capi "endpoint.h value c_orientations" c_c_orientations :: CGameConstant

foreign import ccall "eval_game_constant" c_eval_game_constant :: CGameConstant -> CInt
foreign import ccall "dump_game_constant" c_dump_game_constant :: CGameConstant -> IO ()

cGameConstant_ :: GameConstant -> CGameConstant
cGameConstant_ = \case
	GCColors -> c_c_colors
	GCShapes -> c_c_shapes
	GCWidth -> c_c_width
	GCHeight -> c_c_height
	GCOrientations -> c_c_orientations

cGameConstant :: GameConstant -> IO CGameConstant
cGameConstant = pure . cGameConstant_

hsGameConstant_ :: HasCallStack => CGameConstant -> GameConstant
hsGameConstant_ tag
	| tag == c_c_colors = GCColors
	| tag == c_c_shapes = GCShapes
	| tag == c_c_width = GCWidth
	| tag == c_c_height = GCHeight
	| tag == c_c_orientations = GCOrientations
	| otherwise = error $ "Unknown game constant tag " ++ show tag

hsGameConstant :: HasCallStack => CGameConstant -> IO GameConstant
hsGameConstant = evaluate . hsGameConstant_

evalGameConstant :: GameConstant -> Int
evalGameConstant = \case
	GCColors -> 3
	GCShapes -> 4
	GCWidth -> 8
	GCHeight -> 16
	GCOrientations -> 2

evalCGameConstant :: CGameConstant -> Int
evalCGameConstant = fromIntegral . c_eval_game_constant

dumpGameConstant :: CGameConstant -> IO ()
dumpGameConstant c = hFlush stdout >> c_dump_game_constant c

newtype CDimensionsTag = CDimensionsTag CInt deriving newtype (Eq, Ord, Show, Storable)

foreign import capi "endpoint.h value tag_product" c_tag_product :: CDimensionsTag
foreign import capi "endpoint.h value tag_sum" c_tag_sum :: CDimensionsTag

data Dimensions = Product [GameConstant] | Sum [GameConstant] deriving (Eq, Ord, Read, Show)

evalDimensions :: Dimensions -> [Int]
evalDimensions = \case
	Product gcs -> evalGameConstant <$> gcs
	Sum gcs -> [sum (evalGameConstant <$> gcs)]

newtype CDimensions = CDimensions (ForeignPtr CDimensions)

foreign import ccall "new_dimensions_product" c_new_dimensions_product :: CInt -> IO (Ptr CDimensions)
foreign import ccall "new_dimensions_sum" c_new_dimensions_sum :: CInt -> IO (Ptr CDimensions)
foreign import ccall "dimensions_add_constant" c_dimensions_add_constant :: Ptr CDimensions -> CGameConstant -> IO ()
foreign import ccall "dimensions_get_tag" c_dimensions_get_tag :: Ptr CDimensions -> IO CDimensionsTag
foreign import ccall "dimensions_read" c_dimensions_read :: Ptr CInt -> Ptr (Ptr CGameConstant) -> Ptr CDimensions -> IO ()
foreign import ccall "dump_dimensions" c_dump_dimensions :: Ptr CDimensions -> IO ()
foreign import ccall "&free_dimensions_constants" c_free_dimensions_constants :: FinalizerPtr CGameConstant
foreign import ccall "&free_dimensions" c_free_dimensions :: FinalizerPtr CDimensions

gcDimensions :: Ptr CDimensions -> IO CDimensions
gcDimensions = fmap CDimensions . newForeignPtr c_free_dimensions

newDimensionsProduct :: CInt -> IO CDimensions
newDimensionsProduct n = c_new_dimensions_product n >>= gcDimensions

newDimensionsSum :: CInt -> IO CDimensions
newDimensionsSum n = c_new_dimensions_sum n >>= gcDimensions

dimensionsAddConstant :: CDimensions -> CGameConstant -> IO ()
dimensionsAddConstant (CDimensions fp) gc = withForeignPtr fp \d -> c_dimensions_add_constant d gc

cDimensions :: Dimensions -> IO CDimensions
cDimensions = \case
	Product gcs -> do
		cd <- newDimensionsProduct (clength gcs)
		cd <$ traverse_ (dimensionsAddConstant cd . cGameConstant_) gcs
	Sum gcs -> do
		cd <- newDimensionsSum (clength gcs)
		cd <$ traverse_ (dimensionsAddConstant cd . cGameConstant_) gcs

dimensionsGetTag :: CDimensions -> IO CDimensionsTag
dimensionsGetTag (CDimensions d) = withForeignPtr d c_dimensions_get_tag

dimensionsRead :: CDimensions -> IO (Vector CGameConstant)
dimensionsRead (CDimensions _d) =
	alloca \psize ->
	alloca \pconstants ->
	withForeignPtr _d \d -> do
	c_dimensions_read psize pconstants d
	liftA2 V.unsafeFromForeignPtr0
		(peek pconstants >>= newForeignPtr c_free_dimensions_constants)
		(fromIntegral <$> peek psize)

hsDimensions :: HasCallStack => CDimensions -> IO Dimensions
hsDimensions d = do
	tag <- dimensionsGetTag d
	cs <- map hsGameConstant_ . V.toList <$> dimensionsRead d
	if
		| tag == c_tag_product -> pure (Product cs)
		| tag == c_tag_sum -> pure (Sum cs)
		| otherwise -> error $ "Unknown dimensions tag " ++ show tag

dumpDimensions :: CDimensions -> IO ()
dumpDimensions (CDimensions d) = hFlush stdout >> withForeignPtr d c_dump_dimensions

newtype CStructureTag = CStructureTag CInt deriving newtype (Eq, Ord, Show, Storable)

foreign import capi "endpoint.h value tag_unit" c_tag_unit :: CStructureTag
foreign import capi "endpoint.h value tag_positive" c_tag_positive :: CStructureTag
foreign import capi "endpoint.h value tag_categorical" c_tag_categorical :: CStructureTag
foreign import capi "endpoint.h value tag_masked" c_tag_masked :: CStructureTag
foreign import capi "endpoint.h value tag_rectangle" c_tag_rectangle :: CStructureTag
foreign import capi "endpoint.h value tag_heterogeneous" c_tag_heterogeneous :: CStructureTag

data LeafType = Unit | Positive | Categorical deriving (Bounded, Enum, Eq, Ord, Read, Show)

cLeafType_ :: LeafType -> CStructureTag
cLeafType_ = \case
	Unit -> c_tag_unit
	Positive -> c_tag_positive
	Categorical -> c_tag_categorical

cLeafType :: LeafType -> IO CStructureTag
cLeafType = pure . cLeafType_

hsLeafTypeM_ :: CStructureTag -> Maybe LeafType
hsLeafTypeM_ tag
	| tag == c_tag_unit = Just Unit
	| tag == c_tag_positive = Just Positive
	| tag == c_tag_categorical = Just Categorical
	| otherwise = Nothing

hsLeafType :: HasCallStack => CStructureTag -> IO LeafType
hsLeafType c_tag = case hsLeafTypeM_ c_tag of
	Just tag -> pure tag
	Nothing -> evaluate . error $ "Tried to interpret structure tag " ++ show c_tag ++ " as a leaf type, but it wasn't one."

data Structure
	= SLeaf LeafType
	| SMasked Structure
	| SRectangle Dimensions Structure
	| SHeterogeneous [(String, Structure)]
	deriving (Eq, Ord, Read, Show)

newtype CStructure = CStructure (ForeignPtr CStructure)

foreign import ccall "new_structure_unit" c_new_structure_unit :: IO (Ptr CStructure)
foreign import ccall "new_structure_positive" c_new_structure_positive :: IO (Ptr CStructure)
foreign import ccall "new_structure_categorical" c_new_structure_categorical :: IO (Ptr CStructure)
foreign import ccall "new_structure_masked" c_new_structure_masked :: Ptr CStructure -> IO (Ptr CStructure)
foreign import ccall "new_structure_rectangle" c_new_structure_rectangle :: Ptr CDimensions -> Ptr CStructure -> IO (Ptr CStructure)
foreign import ccall "new_structure_heterogeneous" c_new_structure_heterogeneous :: IO (Ptr CStructure)
foreign import ccall "structure_add_child" c_structure_add_child :: Ptr CStructure -> Ptr CChar -> Ptr CStructure -> IO ()
foreign import ccall "dump_structure" c_dump_structure :: Ptr CStructure -> IO ()
foreign import ccall "&free_structure" c_free_structure :: FinalizerPtr CStructure

gcStructure :: Ptr CStructure -> IO CStructure
gcStructure = fmap CStructure . newForeignPtr c_free_structure

newStructureUnit :: IO CStructure
newStructureUnit = c_new_structure_unit >>= gcStructure

newStructurePositive :: IO CStructure
newStructurePositive = c_new_structure_positive >>= gcStructure

newStructureCategorical :: IO CStructure
newStructureCategorical = c_new_structure_categorical >>= gcStructure

newStructureMasked :: CStructure -> IO CStructure
newStructureMasked (CStructure fp) = withForeignPtr fp \s -> c_new_structure_masked s >>= gcStructure

newStructureRectangle :: CDimensions -> CStructure -> IO CStructure
newStructureRectangle (CDimensions dfp) (CStructure sfp) =
	withForeignPtr dfp \d ->
	withForeignPtr sfp \s ->
	c_new_structure_rectangle d s >>= gcStructure

newStructureHeterogeneous :: IO CStructure
newStructureHeterogeneous = c_new_structure_heterogeneous >>= gcStructure

structureAddChild :: CStructure -> String -> CStructure -> IO ()
structureAddChild (CStructure fpParent) hsName (CStructure fpChild) =
	withForeignPtr fpParent \parent ->
	withCString hsName \name ->
	withForeignPtr fpChild \child ->
	c_structure_add_child parent name child

cStructure :: Structure -> IO CStructure
cStructure = \case
	SLeaf Unit -> newStructureUnit
	SLeaf Positive -> newStructurePositive
	SLeaf Categorical -> newStructureCategorical
	SMasked s -> cStructure s >>= newStructureMasked
	SRectangle d s -> liftJ2 newStructureRectangle (cDimensions d) (cStructure s)
	SHeterogeneous dict -> do
		parent <- newStructureHeterogeneous
		parent <$ for_ dict \(nm, child) -> cStructure child >>= structureAddChild parent nm

dumpStructure :: CStructure -> IO ()
dumpStructure (CStructure s) = hFlush stdout >> withForeignPtr s c_dump_structure

clength :: [a] -> CInt
clength = fromIntegral . length

data StridedVector a = StridedVector
	{ strides :: [Int]
	, bounds :: [Int]
	, contents :: Vector a
	} deriving (Eq, Ord, Read, Show)

lenStridesFor :: [Int] -> [Int]
lenStridesFor = scanr (*) 1

stridesFor :: [Int] -> [Int]
stridesFor = tail . lenStridesFor

indexFor :: HasCallStack => [Int] -> Int -> [Int]
indexFor (s:ss) i = q : indexFor ss r where (q, r) = i `quotRem` s
indexFor [] 0 = []
indexFor [] n = error $ "Index was not a multiple of the smallest stride; leftover bit was " ++ show n

generate :: Storable a => [Int] -> ([Int] -> a) -> StridedVector a
generate bs f = StridedVector
	{ strides = ss
	, bounds = bs
	, contents = V.generate len (f . indexFor ss)
	} where
	len:ss = lenStridesFor bs

generateM :: (Storable a, Monad m) => [Int] -> ([Int] -> m a) -> m (StridedVector a)
generateM bs f = StridedVector ss bs <$> V.generateM len (f . indexFor ss) where
	len:ss = lenStridesFor bs

the :: (HasCallStack, Storable a) => StridedVector a -> a
the StridedVector { bounds = bs, contents = c } = case bs of
	[] -> c V.! 0
	_ -> error $ "Tried to retrieve the unique element from a strided vector with non-empty bounds " ++ show bs ++ ".\nThis is almost always a bug, even if those bounds really do result in just one element."

infixl 9 !
(!) :: (HasCallStack, Storable a) => StridedVector a -> Int -> StridedVector a
StridedVector { strides = s:ss, bounds = b:bs, contents = c } ! i | 0 <= i && i < b = StridedVector
	{ strides = ss
	, bounds = bs
	, contents = V.drop (i*s) c
	}
sv ! i = error $ "Indexing error while computing StridedVector { strides = " ++ show (strides sv) ++ ", bounds = " ++ show (bounds sv) ++ ", contents = <elided> } ! " ++ show i

-- TODO: perhaps we should just eliminate the Leaf constructor entirely (and from the C++, too)?
data MaskedEndpoint
	= MLeaf LeafType (Vector CFloat) (Vector CChar)
	| MRectangle LeafType Dimensions (StridedVector CFloat) (StridedVector CChar)
	| MHeterogeneous [(String, MaskedEndpoint)]
	deriving (Eq, Ord, Read, Show)

maskedEndpointBatchSize :: HasCallStack => MaskedEndpoint -> Int
maskedEndpointBatchSize = \case
	MLeaf _ v _ -> V.length v
	MRectangle _ _ StridedVector { bounds = b:_ } _ -> b
	MHeterogeneous ((_, me):_) -> maskedEndpointBatchSize me
	MHeterogeneous [] -> error "Empty heterogeneous endpoints have indeterminate batch size"

data Endpoint
	= ELeaf LeafType (Vector CFloat)
	| EMasked MaskedEndpoint
	| ERectangle LeafType Dimensions (StridedVector CFloat)
	| EHeterogeneous [(String, Endpoint)]
	deriving (Eq, Ord, Read, Show)

endpointBatchSize :: HasCallStack => Endpoint -> Int
endpointBatchSize = \case
	ELeaf _ v -> V.length v
	EMasked me -> maskedEndpointBatchSize me
	ERectangle _ _ StridedVector { bounds = b:_ } -> b
	EHeterogeneous ((_, e):_) -> endpointBatchSize e
	EHeterogeneous [] -> error "Empty heterogeneous endpoints have indeterminate batch size"

unmask :: MaskedEndpoint -> Endpoint
unmask = \case
	MLeaf ty vs _ -> ELeaf ty vs
	MRectangle ty dims vs _ -> ERectangle ty dims vs
	MHeterogeneous dict -> EHeterogeneous [(nm, unmask me) | (nm, me) <- dict]

newtype CEndpoint = CEndpoint (ForeignPtr CEndpoint)

foreign import ccall "new_endpoint_unit" c_new_endpoint_unit :: CInt -> Ptr CFloat -> Ptr CChar -> IO (Ptr CEndpoint)
foreign import ccall "new_endpoint_positive" c_new_endpoint_positive :: CInt -> Ptr CFloat -> Ptr CChar -> IO (Ptr CEndpoint)
foreign import ccall "new_endpoint_categorical" c_new_endpoint_categorical :: CInt -> Ptr CFloat -> Ptr CChar -> IO (Ptr CEndpoint)
foreign import ccall "new_endpoint_masked" c_new_endpoint_masked :: Ptr CEndpoint -> IO (Ptr CEndpoint)
foreign import ccall "new_endpoint_rectangle" c_new_endpoint_rectangle :: CInt -> CStructureTag -> Ptr CDimensions -> Ptr CFloat -> Ptr CChar -> IO (Ptr CEndpoint)
foreign import ccall "new_endpoint_heterogeneous" c_new_endpoint_heterogeneous :: CInt -> IO (Ptr CEndpoint)
foreign import ccall "endpoint_add_child" c_endpoint_add_child :: Ptr CEndpoint -> Ptr CChar -> Ptr CEndpoint -> IO ()
foreign import ccall "endpoint_get_tag" c_endpoint_get_tag :: Ptr CEndpoint -> IO CStructureTag
foreign import ccall "endpoint_get_child_names" c_endpoint_get_child_names :: Ptr CInt -> Ptr (Ptr (Ptr CChar)) -> Ptr CEndpoint -> IO ()
foreign import ccall "endpoint_get_named_child" c_endpoint_get_named_child :: Ptr CEndpoint -> Ptr CChar -> IO (Ptr CEndpoint)
foreign import ccall "endpoint_get_masked_child" c_endpoint_get_masked_child :: Ptr CEndpoint -> IO (Ptr CEndpoint)
foreign import ccall "endpoint_get_dimensions" c_endpoint_get_dimensions :: Ptr CEndpoint -> IO (Ptr CDimensions)
foreign import ccall "endpoint_read" c_endpoint_read :: Ptr CStructureTag -> Ptr CInt -> Ptr (Ptr CFloat) -> Ptr CEndpoint -> IO ()
foreign import ccall "dump_endpoint" c_dump_endpoint :: Ptr CEndpoint -> IO ()
foreign import ccall "free_endpoint_names" c_free_endpoint_names :: CInt -> Ptr (Ptr CChar) -> IO ()
foreign import ccall "&free_endpoint_values" c_free_endpoint_values :: FinalizerPtr CFloat
foreign import ccall "&free_endpoint" c_free_endpoint :: FinalizerPtr CEndpoint

gcEndpointValues :: Ptr CFloat -> IO (ForeignPtr CFloat)
gcEndpointValues = newForeignPtr c_free_endpoint_values

gcEndpoint :: Ptr CEndpoint -> IO CEndpoint
gcEndpoint = fmap CEndpoint . newForeignPtr c_free_endpoint

unsafeWithMask :: HasCallStack => Int -> Maybe (Vector CChar) -> (Ptr CChar -> IO b) -> IO b
unsafeWithMask sz mv f = case mv of
	Nothing -> f nullPtr
	Just v | sz == V.length v -> V.unsafeWith v f
	       | otherwise -> evaluate . error $ "Size mismatch between values (" ++ show sz ++ ") and mask (" ++ show (V.length v) ++ ")"

newEndpointLeaf :: (CInt -> Ptr CFloat -> Ptr CChar -> IO (Ptr CEndpoint)) -> Vector CFloat -> Maybe (Vector CChar) -> IO CEndpoint
newEndpointLeaf new values mask =
	V.unsafeWith values \c_values ->
	unsafeWithMask n mask \c_mask ->
	gcEndpoint =<< new (fromIntegral n) c_values c_mask
	where n = V.length values

newEndpointUnit :: Vector CFloat -> Maybe (Vector CChar) -> IO CEndpoint
newEndpointUnit = newEndpointLeaf c_new_endpoint_unit

newEndpointPositive :: Vector CFloat -> Maybe (Vector CChar) -> IO CEndpoint
newEndpointPositive = newEndpointLeaf c_new_endpoint_positive

newEndpointCategorical :: Vector CFloat -> Maybe (Vector CChar) -> IO CEndpoint
newEndpointCategorical = newEndpointLeaf c_new_endpoint_categorical

newEndpointMasked :: CEndpoint -> IO CEndpoint
newEndpointMasked (CEndpoint fp) = withForeignPtr fp (gcEndpoint <=< c_new_endpoint_masked)

-- | caller is responsible for ensuring that the StridedVectors provided have
-- strides matching the CDimensions provided
newEndpointRectangle :: HasCallStack => CDimensions -> CStructureTag -> StridedVector CFloat -> Maybe (StridedVector CChar) -> IO CEndpoint
newEndpointRectangle (CDimensions dims) tag StridedVector { bounds = ns@(n:_), contents = values } mask | sanityCheck =
	withForeignPtr dims \c_dims ->
	V.unsafeWith values \c_values ->
	unsafeWithMask (product ns) (contents <$> mask) \c_mask ->
	-- can't use newEndpointLeaf because the batch size /= the vector size
	gcEndpoint =<< c_new_endpoint_rectangle (fromIntegral n) tag c_dims c_values c_mask
	where
	sanityCheck = fmap bounds mask `elem` [Nothing, Just ns]

newEndpointHeterogeneous :: Int -> IO CEndpoint
newEndpointHeterogeneous n = gcEndpoint =<< c_new_endpoint_heterogeneous (fromIntegral n)

endpointAddChild :: CEndpoint -> String -> CEndpoint -> IO ()
endpointAddChild (CEndpoint parent) nm (CEndpoint child) =
	withForeignPtr parent \c_parent ->
	withCString nm \c_nm ->
	withForeignPtr child \c_child ->
	c_endpoint_add_child c_parent c_nm c_child

endpointGetTag :: CEndpoint -> IO CStructureTag
endpointGetTag (CEndpoint e) = withForeignPtr e c_endpoint_get_tag

endpointGetChildNames :: CEndpoint -> IO (Int, Ptr CString)
endpointGetChildNames (CEndpoint e) =
	withForeignPtr e \c_e ->
	alloca \psize ->
	alloca \pstrings -> do
	c_endpoint_get_child_names psize pstrings c_e
	liftA2 (,) (fromIntegral <$> peek psize) (peek pstrings)

endpointGetNamedChild :: CEndpoint -> CString -> IO CEndpoint
endpointGetNamedChild (CEndpoint e) nm = withForeignPtr e (flip c_endpoint_get_named_child nm) >>= gcEndpoint

endpointGetMaskedChild :: CEndpoint -> IO CEndpoint
endpointGetMaskedChild (CEndpoint e) = withForeignPtr e c_endpoint_get_masked_child >>= gcEndpoint

endpointGetDimensions :: CEndpoint -> IO CDimensions
endpointGetDimensions (CEndpoint e) = withForeignPtr e c_endpoint_get_dimensions >>= gcDimensions

endpointRead :: HasCallStack => Int -> CEndpoint -> IO (LeafType, Int, Vector CFloat)
endpointRead len (CEndpoint e) =
	alloca \ptag ->
	alloca \psize ->
	alloca \pvalues ->
	withForeignPtr e \c_e -> do
	c_endpoint_read ptag psize pvalues c_e
	tag <- peek ptag >>= hsLeafType
	size <- fromIntegral <$> peek psize
	values <- peek pvalues >>= newForeignPtr c_free_endpoint_values
	pure (tag, size, V.unsafeFromForeignPtr0 values (size * len))

cEndpoint :: HasCallStack => Endpoint -> IO CEndpoint
cEndpoint = \case
	ELeaf Unit vs -> newEndpointUnit vs Nothing
	ELeaf Positive vs -> newEndpointPositive vs Nothing
	ELeaf Categorical vs -> newEndpointCategorical vs Nothing
	EMasked me -> cMaskedEndpoint me
	ERectangle ty dims sv -> do
		c_dims <- cDimensions dims
		-- TODO: swap argument order in newEndpointRectangle
		newEndpointRectangle c_dims (cLeafType_ ty) sv Nothing
	e@(EHeterogeneous dict) -> do
		c_e <- newEndpointHeterogeneous (endpointBatchSize e)
		c_e <$ for_ dict \(nm, child) -> do
			c_child <- cEndpoint child
			endpointAddChild c_e nm c_child

cMaskedEndpoint :: HasCallStack => MaskedEndpoint -> IO CEndpoint
cMaskedEndpoint = \case
	MLeaf Unit vs m -> newEndpointUnit vs (Just m)
	MLeaf Positive vs m -> newEndpointPositive vs (Just m)
	MLeaf Categorical vs m -> newEndpointCategorical vs (Just m)
	MRectangle ty dims vs m -> do
		c_dims <- cDimensions dims
		newEndpointRectangle c_dims (cLeafType_ ty) vs (Just m)
	me@(MHeterogeneous dict) -> do
		c_me <- newEndpointHeterogeneous (maskedEndpointBatchSize me)
		c_me <$ for_ dict \(nm, child) -> do
			c_child <- cMaskedEndpoint child
			endpointAddChild c_me nm c_child

hsEndpoint :: HasCallStack => CEndpoint -> IO Endpoint
hsEndpoint e = do
	c_tag <- endpointGetTag e
	case hsLeafTypeM_ c_tag of
		Just tag -> do
			(_, size, vals) <- endpointRead 1 e
			pure $ ELeaf tag vals
		Nothing
			-- we shouldn't really be returning masked endpoints from the
			-- neural net, but for testing it's handy not to crash here so we
			-- can check some simple roundtripping properties
			| c_tag == c_tag_masked -> endpointGetMaskedChild e >>= hsEndpoint
			| c_tag == c_tag_rectangle -> do
				dims <- hsDimensions =<< endpointGetDimensions e
				let exampleBounds = evalDimensions dims
				(ty, size, vals) <- endpointRead (product exampleBounds) e
				let batchBounds = size : exampleBounds
				pure $ ERectangle ty dims StridedVector
					{ strides = stridesFor batchBounds
					, bounds = batchBounds
					, contents = vals
					}
			| c_tag == c_tag_heterogeneous -> do
				(numChildren, names) <- endpointGetChildNames e
				dict <- for [0..numChildren-1] \i -> do
					pname <- peek (advancePtr names i)
					name <- peekCString pname
					c_child <- endpointGetNamedChild e pname
					child <- hsEndpoint c_child
					pure (name, child)
				c_free_endpoint_names (fromIntegral numChildren) names
				pure $ EHeterogeneous dict

			| otherwise -> error $ "Invalid endpoint tag " ++ show c_tag

dumpEndpoint :: CEndpoint -> IO ()
dumpEndpoint (CEndpoint e) = hFlush stdout >> withForeignPtr e c_dump_endpoint
