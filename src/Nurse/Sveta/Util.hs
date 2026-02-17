module Nurse.Sveta.Util
	( module Nurse.Sveta.Util
	, module Control.Applicative
	, module Control.Concurrent
	, module Control.Exception
	, module Control.Monad
	, module Control.Monad.Fix
	, module Control.Monad.IO.Class
	, module Control.Monad.ST
	, module Data.Aeson
	, module Data.Aeson.Types
	, module Data.Bifunctor
	, module Data.Bits
	, module Data.Char
	, module Data.Coerce
	, module Data.Default
	, module Data.Foldable
	, module Data.Functor
	, module Data.Functor.Compose
	, module Data.Hashable
	, module Data.Int
	, module Data.IORef
	, module Data.Kind
	, module Data.List
	, module Data.Maybe
	, module Data.Monoid
	, module Data.Ord
	, module Data.Semigroup
	, module Data.String
	, module Data.Time
	, module Data.Traversable
	, module Data.Tree
	, module Data.Word
	, module Dr.Mario.Model
	, module Dr.Mario.Pathfinding
	, module Dr.Mario.PP
	, module Dr.Mario.Util
	, module GHC.Stack
	, module Numeric
	, module System.Directory
	, module System.Environment
	, module System.Exit
	, module System.FilePath
	, module System.IO
	, module System.IO.Error
	, module System.IO.Unsafe
	, module System.Mem
	, module System.Process
	, module System.Random.MWC
	, module System.Random.MWC.Distributions
	, module System.Random.Stateful
	, module Text.Printf
	, module Text.Read
	-- we don't export ByteString because there's two of them
	, HashMap, HashSet, IntMap, IntSet, KeyMap, Map, Seq, Set, Text, Vector
	) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.State
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Types
import Data.Bifunctor
import Data.Bits hiding (rotate) -- conflicts with Dr.Mario.Pathfinding
import Data.Char
import Data.Coerce
import Data.Default
import Data.Foldable
import Data.Functor
import Data.Functor.Compose
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Int
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.IORef
import Data.Kind
import Data.List
import Data.Maybe
import Data.Map (Map)
import Data.Monoid hiding (First(..), Last(..)) -- conflicts with Data.Semigroup
import Data.Ord hiding (Down(..)) -- conflicts with Dr.Mario.Pathfinding
import Data.Semigroup hiding (First(..), Last(..)) -- conflicts with Data.Monoid
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.String
import Data.Text (Text)
import Data.Time
import Data.Traversable
import Data.Tree
import Data.Vector (Vector)
import Data.Vector.Instances
import Data.Word
import Dr.Mario.Model
import Dr.Mario.Pathfinding hiding (MidStep(..), rotate) -- MidStep(Down) conflicts with Data.Ord; rotate conflicts with Data.Bits
import Dr.Mario.PP
import Dr.Mario.Util
import GHC.Stack
import Numeric
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.IO.Unsafe
import System.Mem
import System.Process
import System.Random.MWC
import System.Random.MWC.Distributions
import System.Random.Stateful (uniformFloat01M, uniformDouble01M)
import Text.Printf
import Text.Read (readMaybe)

import qualified Data.Sequence as Seq

infixr 1 ?
(?) :: Bool -> a -> Maybe a
True  ? a = Just a
False ? _ = Nothing

enumerate :: (Traversable t, Num n) => t a -> t (n, a)
enumerate t = evalState (traverse (\a -> state (\i -> ((i, a), i+1))) t) 0

ignored :: HasCallStack => a
ignored = error "a term was not ignored, but it was supposed to be"

-- the arguments should always have been in this order
forZipWithM :: Applicative f => [a] -> [b] -> (a -> b -> f c) -> f [c]
forZipWithM as bs f = zipWithM f as bs

forZipWithM_ :: Applicative f => [a] -> [b] -> (a -> b -> f c) -> f ()
forZipWithM_ as bs f = zipWithM_ f as bs

forZipWith :: [a] -> [b] -> (a -> b -> c) -> [c]
forZipWith as bs f = zipWith f as bs

liftJ2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJ2 f ma mb = join (liftM2 f ma mb)

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f a a' = if f a < f a' then a' else a

minOn :: Ord b => (a -> b) -> a -> a -> a
minOn f a a' = if f a > f a' then a' else a

hay :: Foldable f => f a -> Bool
hay = not . null

foldb :: (a -> a -> a) -> a -> [a] -> a
foldb plus = go where
	pairwise (a:a':rest) = plus a a' : pairwise rest
	pairwise shortList = shortList
	go a [] = a
	go a (a':as) = go (plus a a') (pairwise as)

-- | On my machine, torch and gtk fight over the GPU. This environment variable
-- setting instructs gtk not to do hardware acceleration -- letting torch win
-- the fight.
torchPlusGtkFix :: IO ()
torchPlusGtkFix = lookupEnv "GSK_RENDERER" >>= \case
	Nothing -> setEnv "GSK_RENDERER" "cairo"
	_ -> pure ()

ensure :: Alternative f => (a -> Bool) -> a -> f a
ensure p x = x <$ guard (p x)

-- carefully supports multiple-use partial-application
equating :: Eq b => (a -> b) -> a -> a -> Bool
equating f a = (f a==) . f

frst :: (a, b, c) -> a
frst (a, _, _) = a

scnd :: (a, b, c) -> b
scnd (_, b, _) = b

thrd :: (a, b, c) -> c
thrd (_, _, c) = c

fforever :: Monad m => a -> (a -> m a) -> m b
fforever = flip (fix . (>=>))

reflectError :: Show e => IO (Either e a) -> IO a
reflectError = (>>= either (fail . show) pure)

defOr :: Default a => Maybe a -> a
defOr = fromMaybe def

-- Data.Sequence.index doesn't have a HasCallstack constraint, making it a bit
-- hard to track down issues with its use
seqIndex :: HasCallStack => Seq a -> Int -> a
seqIndex s i = fromJust (s Seq.!? i)
