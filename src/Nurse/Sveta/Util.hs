module Nurse.Sveta.Util where

import Control.Monad
import Control.Monad.State
import Data.Traversable

infixr 1 ?
(?) :: Bool -> a -> Maybe a
True  ? a = Just a
False ? _ = Nothing

enumerate :: (Traversable t, Num n) => t a -> t (n, a)
enumerate t = evalState (traverse (\a -> state (\i -> ((i, a), i+1))) t) 0

-- the arguments should always have been in this order
forZipWithM :: Applicative f => [a] -> [b] -> (a -> b -> f c) -> f [c]
forZipWithM as bs f = zipWithM f as bs

forZipWithM_ :: Applicative f => [a] -> [b] -> (a -> b -> f c) -> f ()
forZipWithM_ as bs f = zipWithM_ f as bs

forZipWith :: [a] -> [b] -> (a -> b -> c) -> [c]
forZipWith as bs f = zipWith f as bs

liftJ2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJ2 f ma mb = join (liftM2 f ma mb)
