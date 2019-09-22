module Dr.Mario.Sveta.Cache where

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import GHC.Stack
import qualified Data.HashMap.Strict as HM

data Shared id a = Shared id | Unshared a deriving (Eq, Ord, Read, Show)
newtype Cache id a = Cache (HashMap id a) deriving (Eq, Ord, Read, Show)

empty :: Cache id a
empty = Cache HM.empty

share :: (Hashable id, Eq id) => id -> a -> Cache id a -> Cache id a
share id a (Cache cache) = Cache (HM.insert id a cache)

deref :: (Hashable id, Eq id) => Cache id a -> Shared id a -> Maybe a
deref _ (Unshared a) = Just a
deref (Cache cache) (Shared id) = HM.lookup id cache

(!) :: (Hashable id, Eq id, HasCallStack) => Cache id a -> Shared id a -> a
c ! s = case deref c s of
	Just a -> a
