{-# Language AllowAmbiguousTypes #-}

module Nurse.Sveta.Torch.EndpointMap where

import Data.Coerce
import Data.Foldable
import Dr.Mario.Model
import Foreign
import GHC.Stack
import Nurse.Sveta.Torch.Endpoint (StridedVector, GameConstant(..), evalGameConstant)
import qualified Nurse.Sveta.Torch.Endpoint as SV

newtype EndpointMap k v = EndpointMap { getEndpointMap :: StridedVector v }
	deriving (Eq, Ord, Read, Show)

infixl 9 !
(!) :: (HasCallStack, EndpointKey k, Storable v, Coercible v v') => EndpointMap k v -> k -> v'
EndpointMap sv ! v = coerce (SV.the (foldl' (SV.!) sv (toIndices v)))

generate :: forall k v. (EndpointKey k, Storable v) => (k -> v) -> EndpointMap k v
generate f = EndpointMap $ SV.generate (evalGameConstant <$> indexCounts @k) (f . fromIndices)

map :: (EndpointKey k, Storable a, Storable b) => (a -> b) -> EndpointMap k a -> EndpointMap k b
map f (EndpointMap v) = EndpointMap (SV.svMap f v)

-- | @toIndices . fromIndices = id@ is a law, but @fromIndices . toIndices =
-- id@ is not (e.g. see the 'Shape' instance, where 'North', 'South', and
-- 'Disconnected' all have the same index).
class (Eq a, Show a) => EndpointKey a where
	indexCounts :: [GameConstant]
	toIndices :: a -> [Int]
	fromIndices :: HasCallStack => [Int] -> a

	default toIndices :: Enum a => a -> [Int]
	toIndices = (:[]) . fromEnum

	default fromIndices :: (HasCallStack, Enum a) => [Int] -> a
	fromIndices [i] = toEnum i
	fromIndices is = error $ "Expected one index to decode, but saw " ++ show (length is) ++ ", namely, " ++ show is

indexCounts' :: forall a. EndpointKey a => [Int]
indexCounts' = evalGameConstant <$> indexCounts @a

toIndex :: forall a. EndpointKey a => a -> Int
toIndex = go (indexCounts' @a) . toIndices where
	go (c:cs) (i:is) = i + c * go cs is
	go [] [] = 0

fromIndex :: forall a. (HasCallStack, EndpointKey a) => Int -> a
fromIndex = fromIndices . go (indexCounts' @a) where
	go (c:cs) i = let (q, r) = i `quotRem` c in r : go cs q
	go [] 0 = []

instance EndpointKey Orientation where indexCounts = [GCOrientations]
instance EndpointKey Color where indexCounts = [GCColors]
instance EndpointKey Shape where
	indexCounts = [GCShapes]
	toIndices = \case
		Virus -> [0]
		West -> [1]
		East -> [2]
		_ -> [3] -- Disconnected, North, and South all have the same strategic content
	fromIndices = \case
		[0] -> Virus
		[1] -> West
		[2] -> East
		[3] -> Disconnected

instance EndpointKey Lookahead where
	indexCounts = indexCounts @Color ++ indexCounts @Color
	toIndices lk = toIndices (leftColor lk) ++ toIndices (rightColor lk)
	fromIndices [l, r] = Lookahead
		{ leftColor = fromIndices [l]
		, rightColor = fromIndices [r]
		}

instance EndpointKey PillContent where
	indexCounts = indexCounts @Orientation ++ indexCounts @Color ++ indexCounts @Color
	toIndices pc = toIndices (orientation pc) ++ toIndices (bottomLeftColor pc) ++ toIndices (otherColor pc)
	fromIndices [o, bl, oc] = PillContent
		{ orientation = fromIndices [o]
		, bottomLeftColor = fromIndices [bl]
		, otherColor = fromIndices [oc]
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
