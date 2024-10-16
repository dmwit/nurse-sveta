{-# Language AllowAmbiguousTypes #-}

module Nurse.Sveta.Torch.Semantics (
	module Nurse.Sveta.Torch.Endpoint,
	-- * Neural net interface
	NetInput(..), NetOutput(..), LossScaling(..), GroundTruth(..), TrainingExample(..),
	lsEndpoint,
	-- * ToEndpoint
	ToEndpoint(..),
	ToEndpointRecordDescription(..), toEndpointRecord,
	eqAsFloat,
	ZeroDefault(..), NoDefault(..), ContainerEndpoint(..),
	-- * FromEndpoint
	FromEndpoint(..), fromEndpoint,
	FromEndpointRecordDescription(..), endpointIndexRecord, field,
	-- * EndpointKey
	EndpointKey(..),
	EM.indexCounts', EM.fromIndex, EM.toIndex,
	-- * Miscellaneous
	EndpointMap,
	Structured(..),
	MapLike(..),
	CFloat(..),
	safeLog,
	) where

import Data.Coerce
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Kind
import Data.Map.Strict (Map)
import Dr.Mario.Model
import Foreign.C
import GHC.Stack
import Nurse.Sveta.Torch.Endpoint
import Nurse.Sveta.Torch.EndpointMap (EndpointMap(..), EndpointKey(..))

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Nurse.Sveta.Torch.EndpointMap as EM

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

instance ToEndpoint Float where toEndpoint = toEndpoint . coerce @_ @(_ CFloat)
instance FromEndpoint Float where endpointIndex e i = coerce @CFloat (endpointIndex e i)

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
		full :: forall a. (Eq a, EndpointKey a) => (Cell -> Maybe a) -> Endpoint
		full f = EFullTensor gcs $ generate
			(V.length boards:map evalGameConstant gcs)
			-- it is important to use toIndices rather than fromIndices here,
			-- because the Shape instance collapses multiple shapes to the same
			-- index
			\[i, v, x, y] -> eqAsFloat (Just [v])
				(toIndices <$> f (unsafeGet (boards V.! i) (Position x y)))
			where gcs = indexCounts @a ++ [gcw, gch]
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

eqAsFloat :: Eq a => a -> a -> CFloat
eqAsFloat x y = if x == y then 1 else 0

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

data NetInput = NetInput
	{ niBoard :: Board
	, niFrames :: Int
	, niOriginalVirusCount :: Int
	} deriving (Eq, Ord, Read, Show)

instance Structured NetInput where
	structure = SDictionary $ tail [undefined
		, ("board", structure @Board)
		, ("frames/10,000", STensor Positive [])
		-- Positive isn't really right for log(frames), since it can be as low
		-- as -1, but reporting a leaf type for net *inputs* is a bit odd
		-- anyway and the leaf type will be essentially ignored
		, ("log(frames/10,000)", STensor Positive [])
		, ("sqrt(frames/10,000)", STensor Positive [])
		, ("original virus count/100", STensor Positive [])
		, ("log(original virus count/100)", STensor Positive [])
		, ("1/sqrt(original virus count)", STensor Positive [])
		]

safeLog :: CFloat -> CFloat
safeLog = log . max (exp (-1))

instance ToEndpoint NetInput where
	toEndpoint = toEndpointRecord
		$   "board" :=: niBoard
		:&: "frames/10,000" :=: frames
		:&: "log(frames/10,000)" :=: safeLog . frames
		:&: "sqrt(frames/10,000)" :=: sqrt . frames
		:&: "original virus count/100" :=: (/100) . viruses
		:&: "log(original virus count/100)" :=: safeLog . (/100) . viruses
		:&: "1/sqrt(original virus count)" :=: recip . sqrt . viruses
		where
		frames, viruses :: NetInput -> CFloat
		frames = (/10000) . fromIntegral . niFrames
		viruses = fromIntegral . niOriginalVirusCount

data GroundTruth = GroundTruth
	{ gtPriors :: HashMap Pill Float
	, gtValuation :: Float
	} deriving (Eq, Ord, Read, Show)

instance ToEndpoint GroundTruth where
	toEndpoint = toEndpointRecord
		$   "priors" :=: NoDefault . gtPriors
		:&: "valuation" :=: gtValuation

data NetOutput = NetOutput
	{ noPriors :: EndpointMap Pill CFloat
	, noValuation :: Float
	} deriving (Eq, Ord, Read, Show)

instance Structured NetOutput where
	structure = SDictionary $ tail [undefined
		, ("priors", STensor Categorical (indexCounts @Pill))
		, ("valuation", STensor Unbounded [])
		]

instance FromEndpoint NetOutput where
	endpointIndex = endpointIndexRecord $ pure NetOutput
		<*> field "priors"
		<*> field "valuation"

instance (EndpointKey k, v ~ CFloat) => FromEndpoint (EndpointMap k v) where
	endpointIndex (EFullTensor gcs sv) i | gcs == indexCounts @k = EndpointMap (sv ! i)

data LossScaling = LossScaling
	{ lsPriors :: Float
	, lsValuation :: Float
	} deriving (Eq, Ord, Read, Show)

instance ToEndpoint LossScaling where
	toEndpoint = toEndpointRecord
		$   "priors" :=: lsPriors
		:&: "valuation" :=: lsValuation

lsEndpoint :: LossScaling -> Endpoint
lsEndpoint = toEndpoint . V.singleton

data TrainingExample = TrainingExample
	{ teInput :: NetInput
	, teTruth :: GroundTruth
	} deriving (Eq, Ord, Read, Show)

instance ToEndpoint TrainingExample where
	toEndpoint = toEndpointRecord
		$   "input" :=: teInput
		:&: "ground truth" :=: teTruth
