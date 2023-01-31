module Nurse.Sveta.Util where

import Control.Monad
import Control.Monad.State
import Data.Traversable

enumerate :: (Traversable t, Num n) => t a -> t (n, a)
enumerate t = evalState (traverse (\a -> state (\i -> ((i, a), i+1))) t) 0
