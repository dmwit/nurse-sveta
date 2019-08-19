{-# LANGUAGE FlexibleInstances #-}
module Dr.Mario.Sveta where

import Control.Monad.Trans.State.Strict
import Data.Bits ((.&.))
import Data.Foldable
import Data.Maybe
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.List
import Dr.Mario.Model hiding (pp)
import qualified Data.HashMap.Strict as HM

-- | Is the game checking whether the down button is pressed this frame?
data Parity = Checking | Ignoring deriving (Bounded, Enum, Eq, Ord, Read, Show)
instance Hashable Parity where hashWithSalt = hashUsing fromEnum

notParity :: Parity -> Parity
notParity Checking = Ignoring
notParity Ignoring = Checking

-- TODO: only do one kind of rotation; for horizontal pills, track where there
-- was a rotation or not (with the existence of a rotation being "better" than
-- no rotation); don't need to track anything extra for vertical pills
--
-- Beware: rotations might fail in 'advance'! Update things carefully correctly.
data Move = Move
	{ direction :: Maybe Direction
	, rotation :: Maybe Rotation
	} deriving (Eq, Ord, Show)

-- | The parts of a 'PillControlState' which can't be improved by changing
-- them.
data PillControlStateEq = PillControlStateEq
	{ pill :: !Pill
	, parity :: !Parity
	-- | The parity we'll get if we wait for a forced drop.
	--
	-- Technically, it's probably more correct to store the actual 'forcedDrop'
	-- in here! This is an approximation that, very roughly, assumes it's very
	-- rare to need to wait for a forced drop, and so never does. With this
	-- approximation, this field rarely matters; only when the pill speed is
	-- very high (and so forced drops actually happen fast enough that we may
	-- not be able to move as far to the right or left as we want before it
	-- comes) will there be two keys with the same 'pill' and 'parity' but
	-- different 'forcedDropParity's.
	--
	-- The approximation is made for efficiency, of course: we can get away
	-- with a much smaller hashmap if we store just a 'Parity' instead of an
	-- 'Int' that, on low speeds, might take on all the values from 0 to
	-- 40ish...
	, forcedDropParity :: !Parity
	} deriving (Eq, Ord, Read, Show)

instance Hashable PillControlStateEq where
	hashWithSalt s (PillControlStateEq pill parity dropParity) = s
		`hashWithSalt` pill
		`hashWithSalt` parity
		`hashWithSalt` dropParity

-- | The parts of a 'PillControlState' which in some cases could be improved.
data PillControlStateHysteresis = PillControlStateHysteresis
	-- forbidden is not strict: we only build these with already-evaluated
	-- things, so the extra seq would be a waste
	{ forbidden :: Move
	, cost :: !Int
	, forcedDrop :: !Int
	} deriving (Eq, Ord, Show)

-- | Invariant: the nested 'forcedDropParity' and 'parity' fields are equal iff
-- the nested 'forcedDrop' field is even.
data PillControlState = PillControlState
	{ pcsEq :: PillControlStateEq
	, pcsHysteresis :: PillControlStateHysteresis
	} deriving (Eq, Ord, Show)

data PartialOrdering = Lt | Eq | Gt | In deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Semigroup PartialOrdering where
	Eq <> x = x
	x <> Eq = x
	Lt <> Lt = Lt
	Gt <> Gt = Gt
	_ <> _ = In

instance Monoid PartialOrdering where mempty = Eq

fromOrdering :: Ordering -> PartialOrdering
fromOrdering LT = Lt
fromOrdering EQ = Eq
fromOrdering GT = Gt

-- | 'Lt' is better. This class was basically invented specifically for the
-- 'PillControlState' instance; the behavior of all the other instances was
-- made to support that one and shouldn't be relied on.
class POrd a where
	pcompare :: a -> a -> PartialOrdering

pcompareOrd :: Ord a => a -> a -> PartialOrdering
pcompareOrd a b = fromOrdering (compare a b)

pcompareEq :: Eq a => a -> a -> PartialOrdering
pcompareEq a b = if a == b then Eq else In

instance POrd Direction where pcompare = pcompareEq
instance POrd Rotation where pcompare = pcompareEq
-- | Having 'Nothing' forbidden is better than having something forbidden.
instance POrd a => POrd (Maybe a) where
	pcompare Nothing Nothing = Eq
	pcompare Nothing (Just b) = Lt
	pcompare (Just a) Nothing = Gt
	pcompare (Just a) (Just b) = pcompare a b
instance POrd Move where
	pcompare m1 m2 =  pcompare (direction m1) (direction m2)
	               <> pcompare (rotation m1) (rotation m2)
instance POrd PillControlStateEq where pcompare = pcompareEq
instance POrd PillControlStateHysteresis where
	pcompare (PillControlStateHysteresis f1 c1 d1)
	         (PillControlStateHysteresis f2 c2 d2)
		=  pcompare f1 f2
		<> pcompareOrd c1 c2
		<> pcompareOrd d2 d1 -- backwards! having more time before you're forced to drop is better
instance POrd PillControlState where
	pcompare (PillControlState e1 h1)
	         (PillControlState e2 h2)
		=  pcompare e1 e2
		<> pcompare h1 h2

movesAvailable :: Board -> PillControlState -> [Move]
movesAvailable b (PillControlState
	PillControlStateEq { pill = p, parity = par }
	PillControlStateHysteresis { forbidden = m, forcedDrop = fd }) =
	[ Move d r
	| d <- [Just down  | checking && hasSpace down]
	    ++ [Nothing    | not checking] -- don't have a seizure
	    ++ [Just left  | mustDown || (canPress left && (complicatedLeft || hasSpace left))]
	    ++ [Just right | mustDown || (canPress right && hasSpace right)]
	    ++ [Nothing    | checking]
	, r <- Nothing : map Just (maybe id delete (rotation m) [Clockwise, Counterclockwise])
	]
	where
	checking = par == Checking
	canPress dir = direction m /= Just dir
	hasSpace dir = isJust (move b p dir)
	-- The conditions under which a left-and-rotate maneuver are sensible to
	-- try are too complicated to get right. Just always make it available.
	complicatedLeft = Vertical == orientation (content p)
	-- The conditions under which left or right moves are allowed after a
	-- natural drop is too complicated to get right. Just make them always
	-- available if a forced drop is about to happen.
	mustDown = fd == 1

{-# INLINE even' #-}
even' :: Int -> Bool
even' n = 0 == n .&. 1

-- | The 'Int' is the pill speed.
--
-- This function "takes your word for it" on the 'Move' argument; it does not
-- check whether:
--
-- * the given 'Move' conflicts with the 'forbidden' field of the
--   'PillControlState',
-- * the 'direction' of the 'Move' conflicts with the 'parity' of the
--   'PillControlState', or
-- * whether 'down' moves would cause the pill to lock instead of move.
--
-- ...but see also 'movesAvailable', which does do these checks.
advance :: Board -> Int -> PillControlState -> Move -> PillControlState
advance b d (PillControlState e h) m = PillControlState
	PillControlStateEq
		{ pill = id
		       -- The order of operations here was determined experimentally (i.e.
		       -- not by reading the assembly).
		       . try move bonusMove
		       . try rotate (rotation m)
		       . try move (direction m)
		       . try move naturalDrop
		       $ pill e
		, parity = notParity (parity e)
		, forcedDropParity = if dropThisFrame
			then if even' d
				then notParity (parity e)
				else parity e
			else forcedDropParity e
		}
	PillControlStateHysteresis
		{ forbidden = m
		, cost = cost h + 1
		, forcedDrop = if dropThisFrame then d else forcedDrop h - 1
		}
	where
	try f x p = fromMaybe p (x >>= f b p)

	-- Famous-ish bug: pressing rotate and left on the same frame when the pill
	-- is vertical gets you a bonus left move.
	bonusMove = case (orientation (content (pill e)), m) of
		(Vertical, Move (Just dir) (Just _)) | dir == left -> Just left
		_ -> Nothing
	naturalDrop
		| naturalDropThisFrame && not requestedDropThisFrame = Just down
		| otherwise = Nothing

	dropThisFrame = naturalDropThisFrame || requestedDropThisFrame
	naturalDropThisFrame = forcedDrop h == 1
	requestedDropThisFrame = direction m == Just down

data KeyComparisonResult = HasGtOrEq | HasLt | OnlyIn deriving (Bounded, Enum, Eq, Ord, Read, Show)

compareKeys :: (a -> PartialOrdering) -> [a] -> KeyComparisonResult
compareKeys f [] = OnlyIn
compareKeys f (a:as) = case f a of
	Lt -> HasLt
	Eq -> HasGtOrEq
	Gt -> HasGtOrEq
	In -> compareKeys f as

-- | Use of this type synonym implies that any two elements of the list have
-- their keys 'pcompare' as 'In'comparable.
type MinMap k v = [(k, v)]

-- | Insert a key-value pair into a 'MinMap', deleting any existing key-value
-- pairs whose key is greater than the one currently being inserted. Does
-- nothing if an equal or lesser key is already in the map.
--
-- The 'Bool' returned indicates whether the resulting 'MinMap' is different
-- from the input.
insertMinMap :: POrd k => k -> v -> MinMap k v -> (Bool, MinMap k v)
insertMinMap k v m = case compareKeys (pcompare k . fst) m of
	HasGtOrEq -> (False, m)
	HasLt -> (True, (k,v):filter ((In==) . pcompare k . fst) m)
	OnlyIn -> (True, (k,v):m)

type Visited = HashMap PillControlStateEq (MinMap PillControlStateHysteresis [Move])

-- | Mark a particular 'PillControlState' as visited. The 'Bool' indicates
-- whether we should consider this a first visit.
insertVisited :: PillControlStateEq -> PillControlStateHysteresis -> [Move] -> Visited -> (Bool, Visited)
-- TODO: use alterF instead maybe
insertVisited e h ms v = case HM.lookup e v of
	Just mm -> let (first, mm') = insertMinMap h ms mm in (first, HM.insert e mm' v)
	Nothing -> (True, HM.insert e [(h,ms)] v)

-- TODO: provide a way to cache results across pill drops
reachable :: Board -> Int -> Pill -> Parity -> HashMap Pill [Move]
reachable b dropSpeed initPill initParity = summarize (execState (dfs initPCS []) HM.empty) where
	initPCS = PillControlState
		PillControlStateEq
			{ pill = initPill
			, parity = initParity
			, forcedDropParity = if even' dropSpeed then initParity else notParity initParity
			}
		PillControlStateHysteresis
			{ forbidden = Move Nothing Nothing
			, cost = 0
			, forcedDrop = dropSpeed
			}

	dfs s@(PillControlState e h) ms = do
		(continue, visited) <- gets (insertVisited e h ms)
		if continue
		then put visited >> for_ (movesAvailable b s) (\m -> dfs (advance b dropSpeed s m) (m:ms))
		else return ()

	summarize = id
		. fmap (reverse . snd)
		. HM.foldlWithKey' summarize' HM.empty

	summarize' summary e mm = case move b (pill e) down of
		Nothing -> foldl' (\summary' (h, ms) -> HM.insertWith keepMinCost (pill e) (cost h, ms) summary') summary mm
		Just _ -> summary

	keepMinCost v1@(c1, _) v2@(c2, _) = if c1 < c2 then v1 else v2

class PP a where pp :: a -> String
instance PP Position where pp (Position x y) = "(" ++ show x ++ "," ++ show y ++ ")"
instance PP Direction where
	pp d | d == left = "←"
	     | d == right = "→"
	     | d == down = "↓"
instance PP Rotation where
	pp Clockwise = "↻"
	pp Counterclockwise = "↺"
instance PP Orientation where
	pp Vertical = "↕"
	pp Horizontal = "↔"
instance PP Color where
	pp Blue = "b"
	pp Red = "r"
	pp Yellow = "y"
instance PP PillContent where pp (PillContent o c c') = pp o ++ pp c ++ pp c'
instance PP Pill where pp (Pill c pos) = pp c ++ "@" ++ pp pos
instance PP Move where
	pp (Move dir rot) = concat
		[ if isJust dir == isJust rot then "(" else ""
		, foldMap pp dir
		, foldMap pp rot
		, if isJust dir == isJust rot then ")" else ""
		]
instance PP [Move] where pp ms = show (length ms) ++ "/" ++ (ms >>= pp)
instance PP PillControlStateHysteresis where
	pp (PillControlStateHysteresis m c d) = pp m ++ " " ++ show c ++ " " ++ show d
instance PP Parity where
	pp Checking = "✓"
	pp Ignoring = "x"
instance PP PillControlStateEq where
	pp (PillControlStateEq pill p fdp) = unwords [pp pill, pp p, pp fdp]
instance PP PillControlState where
	pp (PillControlState e h) = pp e ++ " " ++ pp h
instance (PP a, PP b) => PP (a, b) where pp (a, b) = pp a ++ ": " ++ pp b
instance PP [(Pill, [Move])] where pp = unlines . map pp
instance PP (HashMap Pill [Move]) where pp = pp . sort . HM.toList
instance PP (MinMap PillControlStateHysteresis [Move]) where
	pp mm = "[" ++ intercalate "," (map pp mm) ++ "]"
instance PP [(PillControlStateEq, MinMap PillControlStateHysteresis [Move])] where pp = unlines . map pp
instance PP Visited where pp = pp . sort . HM.toList
