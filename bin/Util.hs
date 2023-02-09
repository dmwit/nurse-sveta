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
