module Dr.Mario.Sveta.BoardSummary
	( BoardSummary(..)
	, summarizeBoard
	, msummarizeBoard
	, unsafeSummarizeBoard
	, munsafeSummarizeBoard
	) where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Functor
import Data.Hashable
import Data.Word
import Dr.Mario.Model
import Dr.Mario.Model.Internal
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV

-- | A compact representation of all the information needed for making
-- strategic decisions in Dr. Mario (though not in a format that makes it easy
-- to make those decisions!).
--
-- No promises about the exact format of the data contained within. If you try
-- to use anything other than the 'Hashable', 'Eq', or 'Ord' instances, that's
-- on you.
data BoardSummary = BoardSummary
	{ col0, col1, col2, col3, col4, col5, col6, col7 :: {-# UNPACK #-} !Word64 }
	deriving (Eq, Ord, Read, Show)

instance Hashable BoardSummary where
	hashWithSalt s (BoardSummary c0 c1 c2 c3 c4 c5 c6 c7) = s
		`hashWithSalt` c0
		`hashWithSalt` c1
		`hashWithSalt` c2
		`hashWithSalt` c3
		`hashWithSalt` c4
		`hashWithSalt` c5
		`hashWithSalt` c6
		`hashWithSalt` c7

w, h, maxx, maxy :: Int
w = 8
h = 16
maxx = w-1
maxy = h-1

-- | If @summarizeBoard b1 = summarizeBoard b2 = Just s@, then making move @m@
-- on board @b1@ will be exactly as good, strategically-speaking, as making
-- move @m@ on board @b2@. Conversely, if @summarizeBoard b1 = Just s1@ and
-- @summarizeBoard b2 = Just s2@ and @s1 /= s2@, then there is some move which
-- has different strategic consequences on boards @b1@ and @b2@.
summarizeBoard :: Board -> Maybe BoardSummary
summarizeBoard b
	| width b == w && height b == h = Just (unsafeSummarizeBoard b)
	| otherwise = Nothing

-- | Assumes that the given 'Board' is 8x16.
unsafeSummarizeBoard :: Board -> BoardSummary
unsafeSummarizeBoard b = BoardSummary
	(col 0)
	(col 1)
	(col 2)
	(col 3)
	(col 4)
	(col 5)
	(col 6)
	(col 7)
	where
	col i = unsafeSummarizeColumn (V.unsafeIndex (cells b) i)

-- | Assumes that the given vector is exactly 16 cells long.
unsafeSummarizeColumn :: U.Vector Cell -> Word64
unsafeSummarizeColumn = U.ifoldl' (\w i c -> w .|. (summarizeCell c `shiftL` (4*i))) 0

summarizeCell :: Cell -> Word64
summarizeCell c = colorBits .|. shapeBits where
	colorBits = case c of
		Empty -> 0
		Occupied Red _ -> 1
		Occupied Blue _ -> 2
		Occupied Yellow _ -> 3
	shapeBits = case c of
		Empty -> 0
		Occupied _ Virus -> 0
		Occupied _ Disconnected -> 4
		Occupied _ North -> 4
		Occupied _ South -> 4
		Occupied _ East -> 8
		Occupied _ West -> 12

-- | @fmap summarizeBoard . mfreeze@, but more efficient.
msummarizeBoard :: PrimMonad m => MBoard (PrimState m) -> m (Maybe BoardSummary)
msummarizeBoard mb
	| mwidth mb == w && mheight mb == h = Just <$> munsafeSummarizeBoard mb
	| otherwise = pure Nothing

-- | @fmap unsafeSummarizeBoard . mfreeze@, but more efficient.
munsafeSummarizeBoard :: PrimMonad m => MBoard (PrimState m) -> m BoardSummary
munsafeSummarizeBoard mb = pure BoardSummary
	<*> col 0
	<*> col 1
	<*> col 2
	<*> col 3
	<*> col 4
	<*> col 5
	<*> col 6
	<*> col 7
	where
	{-# INLINE col #-}
	col x = foldM cell 0 [i+maxy, i+maxy-1 .. i] where i = x*h
	cell w i = MV.unsafeRead (mcells mb) i <&> \c -> (w `shiftL` 4) .|. summarizeCell c
