{-# Language AllowAmbiguousTypes #-}

module Dr.Mario.Torch (netSample, netEvaluation) where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Traversable
import Dr.Mario.Model
import Dr.Mario.Model.Internal
import Dr.Mario.Tomcats
import Data.Vector (Vector)
import Foreign
import Foreign.C.Types

import qualified Data.Vector.Unboxed.Mutable as MV

foreign import ccall "sample" cxx_sample :: IO (Ptr Net)
foreign import ccall "&discard" cxx_discard :: FunPtr (Ptr Net -> IO ())
foreign import ccall "evaluate" cxx_evaluate :: Ptr Net -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

newtype Net = Net (ForeignPtr Net)

netSample :: IO Net
netSample = Net <$> (cxx_sample >>= newForeignPtr cxx_discard)

class OneHot a where
	indexCount :: Int
	toIndex :: a -> Int
	fromIndex :: Int -> a

instance OneHot Color where
	indexCount = 1 + fromEnum (maxBound :: Color)
	toIndex = fromEnum
	fromIndex = toEnum

instance OneHot Shape where
	indexCount = 4
	toIndex = \case
		Virus -> 0
		East -> 1
		West -> 2
		_ -> 3 -- Disconnected, North, and South all have the same strategic content
	fromIndex = \case
		0 -> Virus
		1 -> East
		2 -> West
		3 -> Disconnected

render :: [(GameState, Color, Color)] -> IO (Ptr CDouble, Ptr CDouble)
render triples = do
	boards <- mallocZeroArray (n * onehotCount)
	lookaheads <- mallocZeroArray (n * 2 * indexCount @Color)
	forZipWithM_ [0..] triples $ \i (GameState { board = b }, l, r) -> do
		unless (mwidth b == 8 && mheight b == 16) . fail $
			"expected all boards to have size 8x16, but saw a board with size " ++ show (mwidth b) ++ "x" ++ show (mheight b)
		let iBoard = i*onehotCount
		    iLookahead = i*6
		pokeElemOff lookaheads (                    iLookahead + toIndex l) 1
		pokeElemOff lookaheads (indexCount @Color + iLookahead + toIndex r) 1
		-- IOBoard stores cells in a 1D array with the y coordinate varying
		-- fastest, just like the tensor we want to make. This means we get to
		-- reuse the index into that array as an index into our array. Nice.
		MV.iforM_ (mcells b) $ \j -> \case
			Empty -> pure ()
			Occupied c s -> do
				-- shiftl x 7 = cellCount * x
				pokeElemOff boards (iBoard + shiftL (                    toIndex c) 7 + j) 1
				pokeElemOff boards (iBoard + shiftL (indexCount @Color + toIndex s) 7 + j) 1
	pure (boards, lookaheads)
	where
	n, onehotCount, cellCount :: Int
	n = length triples
	onehotCount = cellSize*cellCount
	cellSize = indexCount @Color + indexCount @Shape
	cellCount = 8*16

parse :: [(GameState, Color, Color)] -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO [(Double, HashMap PillContent (Vector (Vector Double)))]
parse states priors valuation scalars = undefined

netEvaluation :: Traversable t => Net -> t (GameState, Color, Color) -> IO (t (Double, HashMap PillContent (Vector (Vector Double))))
netEvaluation (Net net) states = do
	partiallyEvaluated <- for states $ \state@(gs, _, _) -> do
		vk <- readIORef (virusesKilled gs)
		if vk == originalVirusCount gs
		then Left <$> dumbEvaluation state
		else pure (Right state)
	let needsNet = foldMap (\case Left{} -> []; Right v -> [v]) partiallyEvaluated
	    n = length needsNet

	priors <- mallocArray (n * 4 * 8 * 16)
	valuation <- mallocArray n
	scalars <- mallocArray (n * 3)
	(boards, lookaheads) <- render needsNet

	withForeignPtr net $ \netPtr -> cxx_evaluate netPtr priors valuation scalars boards lookaheads
	resultRef <- parse needsNet priors valuation scalars >>= newIORef
	fullyEvaluated <- for partiallyEvaluated $ \case
		Left done -> pure done
		Right _ -> do
			result:results <- readIORef resultRef
			writeIORef resultRef results
			pure result

	free priors
	free valuation
	free scalars
	free boards
	free lookaheads

	pure fullyEvaluated

-- the arguments should always have been in this order
forZipWithM_ :: Applicative f => [a] -> [b] -> (a -> b -> f c) -> f ()
forZipWithM_ as bs f = zipWithM_ f as bs

mallocZeroArray :: forall a. Storable a => Int -> IO (Ptr a)
mallocZeroArray n = do
	ptr <- mallocArray n
	fillBytes ptr 0 (n * sizeOf (undefined :: a))
	pure ptr
