{-# Language AllowAmbiguousTypes #-}

module Dr.Mario.Torch (netSample, netEvaluation) where

import Control.Monad
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Traversable
import Dr.Mario.Model
import Dr.Mario.Model.Internal
import Dr.Mario.Tomcats
import Data.Vector (Vector)
import Nurse.Sveta.Util
import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MV

foreign import ccall "sample" cxx_sample :: Bool -> IO (Ptr Net)
foreign import ccall "&discard" cxx_discard :: FunPtr (Ptr Net -> IO ())
foreign import ccall "evaluate" cxx_evaluate :: Ptr Net -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

newtype Net = Net (ForeignPtr Net)

netSample :: Bool -> IO Net
netSample training = Net <$> (cxx_sample training >>= newForeignPtr cxx_discard)

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

render :: Traversable t => t (Int, (GameState, Color, Color)) -> IO (Ptr CDouble, Ptr CDouble)
render itriples = do
	boards <- mallocZeroArray (n * onehotCount)
	lookaheads <- mallocZeroArray (n * 2 * indexCount @Color)
	for_ itriples $ \(i, (GameState { board = b }, l, r)) -> do
		unless (mwidth b == boardWidth && mheight b == boardHeight) . fail $
			"expected all boards to have size " ++ show boardWidth ++ "x" ++ show boardHeight ++ ", but saw a board with size " ++ show (mwidth b) ++ "x" ++ show (mheight b)
		let iBoard = i*onehotCount
		    iLookahead = i*indexCount @Color*2
		pokeElemOff lookaheads (                    iLookahead + toIndex l) 1
		pokeElemOff lookaheads (indexCount @Color + iLookahead + toIndex r) 1
		-- IOBoard stores cells in a 1D array with the y coordinate varying
		-- fastest, just like the tensor we want to make. This means we get to
		-- reuse the index into that array as an index into our array. Nice.
		MV.iforM_ (mcells b) $ \j -> \case
			Empty -> pure ()
			Occupied c s -> do
				pokeElemOff boards (iBoard + shiftL (                    toIndex c) logCellCount + j) 1
				pokeElemOff boards (iBoard + shiftL (indexCount @Color + toIndex s) logCellCount + j) 1
	pure (boards, lookaheads)
	where
	n, onehotCount :: Int
	n = length itriples
	onehotCount = cellSize*cellCount
	cellSize = indexCount @Color + indexCount @Shape

-- TODO: perhaps we could avoid all this realToFrac stuff by just using CDouble everywhere instead of Double...? I mean why not
parseForEvaluation :: Int -> (GameState, Color, Color) -> ForeignPtr CDouble -> ForeignPtr CDouble -> ForeignPtr CDouble -> IO (Double, HashMap PillContent (Vector (Vector Double)))
parseForEvaluation i (_, l, r) priors_ valuation_ scalars_ = withForeignPtrs (priors_, (valuation_, (scalars_, ()))) $ \(priors, (valuation, (scalars, ()))) -> do
	v <- realToFrac <$> peekElemOff valuation i

	let iPriors = shiftL i logNumPriors
	p <- forZipWithM [0..numRotations-1] (iterate (`rotateContent` Clockwise) (PillContent Horizontal l r)) $ \numRots pc -> do
		let iNumRots = iPriors + shiftL numRots logCellCount
		v <- V.generateM boardWidth $ \x -> let ix = iNumRots + shiftL x logBoardHeight in
			V.generateM boardHeight $ \y -> let iy = ix + y in
				realToFrac <$> peekElemOff priors iy
		pure (pc, v)

	pure (v, HM.fromList p)

netEvaluation :: Traversable t => Net -> t (GameState, Color, Color) -> IO (t (Double, HashMap PillContent (Vector (Vector Double))))
netEvaluation (Net net) triples = do
	[priors, valuation, scalars] <- mallocForeignPtrArrays [shiftL n logNumPriors, n, n * numScalars]
	-- TODO: can we avoid a ton of allocation here by pooling allocations of each size -- or even just the largest size, per Net, say?
	(boards, lookaheads) <- render itriples

	withForeignPtrs (net, (priors, (valuation, (scalars, ())))) $ \(netPtr, (priorsPtr, (valuationPtr, (scalarsPtr, ())))) ->
		cxx_evaluate netPtr (fromIntegral n) priorsPtr valuationPtr scalarsPtr boards lookaheads
	result <- for itriples $ \(i, state) ->
			-- unsafeInterleaveIO: turns out parseForEvaluation is a
			-- bottleneck to making foreign calls, so spread its work across
			-- multiple threads by having the consuming thread perform it
			-- rather than this one
			unsafeInterleaveIO (parseForEvaluation i state priors valuation scalars)

	free boards
	free lookaheads

	pure result
	where
	n = length triples
	itriples = enumerate triples

boardWidth, boardHeight, cellCount, numRotations, numPriors, numScalars :: Int
logBoardWidth, logBoardHeight, logCellCount, logNumRotations, logNumPriors :: Int
boardWidth = 8; logBoardWidth = 3
boardHeight = 16; logBoardHeight = 4
numRotations = 4; logNumRotations = 2
cellCount = boardWidth*boardHeight; logCellCount = logBoardWidth+logBoardHeight
numPriors = numRotations*cellCount; logNumPriors = logNumRotations+logCellCount
numScalars = 3 -- virus clears, pills, frames (in that order)

class WithForeignPtrs a where
	type WithoutForeignPtrs a
	withForeignPtrs :: a -> (WithoutForeignPtrs a -> IO b) -> IO b

instance WithForeignPtrs () where
	type WithoutForeignPtrs () = ()
	withForeignPtrs _ f = f ()

instance WithForeignPtrs b => WithForeignPtrs (ForeignPtr a, b) where
	type WithoutForeignPtrs (ForeignPtr a, b) = (Ptr a, WithoutForeignPtrs b)
	withForeignPtrs (fp, fps) f = withForeignPtr fp $ \a -> withForeignPtrs fps $ \b -> f (a, b)

-- the arguments should always have been in this order
forZipWithM :: Applicative f => [a] -> [b] -> (a -> b -> f c) -> f [c]
forZipWithM as bs f = zipWithM f as bs

mallocZeroArray :: forall a. Storable a => Int -> IO (Ptr a)
mallocZeroArray n = do
	ptr <- mallocArray n
	fillBytes ptr 0 (n * sizeOf (undefined :: a))
	pure ptr

plusForeignPtr' :: forall a. Storable a => ForeignPtr a -> Int -> ForeignPtr a
plusForeignPtr' ptr n = plusForeignPtr ptr (n * sizeOf (undefined :: a))

mallocForeignPtrArrays :: Storable a => [Int] -> IO [ForeignPtr a]
mallocForeignPtrArrays lengths = do
	base <- mallocForeignPtrArray (sum lengths)
	pure . init $ scanl plusForeignPtr' base lengths
