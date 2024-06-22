{-# Language AllowAmbiguousTypes #-}

module Nurse.Sveta.Torch.Semantics (
	module Nurse.Sveta.Torch.Endpoint,
	-- * Neural net interface
	NextNetInput(..), NextNetOutput(..), NextLossScaling(..), NextGroundTruth(..), NextTrainingExample(..),
	lsEndpoint,
	-- * ToEndpoint
	ToEndpoint(..),
	ToEndpointRecordDescription(..), toEndpointRecord,
	OneHot(..), MapLike(..),
	eqAsFloat, oneHotValue,
	OneHotScalar(..), ZeroDefault(..), NoDefault(..), ContainerEndpoint(..),
	-- * FromEndpoint
	FromEndpoint(..), fromEndpoint,
	FromEndpointRecordDescription(..), endpointIndexRecord, field,
	-- * Miscellaneous
	Structured(..),
	EndpointKey(..),
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

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Vector as V

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
class (Eq a, Show a) => EndpointKey a where
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

data NextNetInput = NextNetInput
	{ niBoard :: Board
	, niFrames :: Int
	, niOriginalVirusCount :: Int
	} deriving (Eq, Ord, Read, Show)

instance Structured NextNetInput where
	structure = SDictionary $ tail [undefined
		, ("board", structure @Board)
		, ("frames", STensor Positive [])
		-- Positive isn't really right for log(frames), since it can be as low
		-- as -1, but reporting a leaf type for net *inputs* is a bit odd
		-- anyway and the leaf type will be essentially ignored
		, ("log(frames)", STensor Positive [])
		, ("sqrt(frames)", STensor Positive [])
		, ("original virus count", STensor Positive [])
		, ("log(original virus count)", STensor Positive [])
		, ("1/sqrt(original virus count)", STensor Positive [])
		]

safeLog :: CFloat -> CFloat
safeLog = log . max (exp (-1))

instance ToEndpoint NextNetInput where
	toEndpoint = toEndpointRecord
		$   "board" :=: niBoard
		:&: "frames" :=: frames
		:&: "log(frames)" :=: safeLog . frames
		:&: "sqrt(frames)" :=: sqrt . frames
		:&: "original virus count" :=: viruses
		:&: "log(original virus count)" :=: safeLog . viruses
		:&: "1/sqrt(original virus count)" :=: recip . sqrt . viruses
		where
		frames, viruses :: NextNetInput -> CFloat
		frames = fromIntegral . niFrames
		viruses = fromIntegral . niOriginalVirusCount

data NextGroundTruth = NextGroundTruth
	{ gtPriors :: HashMap Pill Float
	, gtLookahead :: Lookahead
	, gtValuation :: Float
	} deriving (Eq, Ord, Read, Show)

instance ToEndpoint NextGroundTruth where
	toEndpoint = toEndpointRecord
		$   "priors" :=: NoDefault . gtPriors
		:&: "valuation" :=: \gt -> NoDefault (HM.singleton (gtLookahead gt) (gtValuation gt))

data NextNetOutput = NextNetOutput
	{ noPriors :: HashMap Pill Float
	, noValuation :: HashMap Lookahead Float
	-- TODO: poke through the stuff in Prediction and see what we should migrate into here
	} deriving (Eq, Ord, Read, Show)

instance Structured NextNetOutput where
	structure = SDictionary $ tail [undefined
		, ("priors", STensor Categorical (indexCounts @Pill))
		, ("valuation", STensor Unit (indexCounts @Lookahead))
		]

instance FromEndpoint NextNetOutput where
	endpointIndex = endpointIndexRecord $ pure NextNetOutput
		<*> field "priors"
		<*> field "valuation"

data NextLossScaling = NextLossScaling
	{ lsPriors :: Float
	, lsValuation :: Float
	} deriving (Eq, Ord, Read, Show)

instance ToEndpoint NextLossScaling where
	toEndpoint = toEndpointRecord
		$   "priors" :=: lsPriors
		:&: "valuation" :=: lsValuation

lsEndpoint :: NextLossScaling -> Endpoint
lsEndpoint = toEndpoint . V.singleton

data NextTrainingExample = NextTrainingExample
	{ teInput :: NextNetInput
	, teTruth :: NextGroundTruth
	} deriving (Eq, Ord, Read, Show)

instance ToEndpoint NextTrainingExample where
	toEndpoint = toEndpointRecord
		$   "input" :=: teInput
		:&: "ground truth" :=: teTruth
