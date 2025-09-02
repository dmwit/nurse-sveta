module Util where

import Control.Applicative
import Control.Monad
import System.Environment

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
