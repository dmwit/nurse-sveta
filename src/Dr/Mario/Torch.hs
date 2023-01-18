module Dr.Mario.Torch (netSample, netEvaluation) where

import Data.HashMap.Strict (HashMap)
import Dr.Mario.Model
import Dr.Mario.Tomcats
import Data.Vector (Vector)

foreign import ccall "ffi_demo" ffi_demo :: IO ()

data Net = Net

netSample :: IO Net
netSample = pure Net

netEvaluation :: Net -> (GameState, Color, Color) -> IO (Double, HashMap PillContent (Vector (Vector Double)))
netEvaluation net state = do
	ffi_demo
	dumbEvaluation state
