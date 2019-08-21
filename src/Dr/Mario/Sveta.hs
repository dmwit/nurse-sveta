{-# LANGUAGE FlexibleInstances #-}
module Dr.Mario.Sveta where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Bits ((.&.))
import Data.Foldable
import Data.Functor
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

-- | Have we seen any runs with an odd number of contiguous rotations, so that
-- we may swap all the rotations in that run to flip the final pill 180
-- degrees?
data RotationFlexibility
	= Inflexible
	| Pending
	| Flexible
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Hashable RotationFlexibility where hashWithSalt = hashUsing fromEnum

-- | The parts of a 'PillControlState' which in some cases could be improved.
data PillControlStateHysteresis = PillControlStateHysteresis
	-- forbidden is not strict: we only build these with already-evaluated
	-- things, so the extra seq would be a waste
	{ forbidden :: Move
	, cost :: !Int
	, forcedDrop :: !Int
	, flexibility :: !RotationFlexibility
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
instance POrd RotationFlexibility where
	pcompare x y | x == y = Eq
	pcompare Flexible _ = Lt
	pcompare _ Flexible = Gt
	pcompare _ _ = In
instance POrd PillControlStateEq where pcompare = pcompareEq
instance POrd PillControlStateHysteresis where
	pcompare (PillControlStateHysteresis f1 c1 d1 x1)
	         (PillControlStateHysteresis f2 c2 d2 x2)
		=  pcompare (direction f1) (direction f2) -- don't pcompare rotations: we allow rotation every frame, and fix things up in a postprocessing step to make sure adjacent ones are opposite directions
		<> pcompareOrd c1 c2
		<> pcompareOrd d2 d1 -- backwards! having more time before you're forced to drop is better
		<> pcompare x1 x2
instance POrd PillControlState where
	pcompare (PillControlState e1 h1)
	         (PillControlState e2 h2)
		=  pcompare e1 e2
		<> pcompare h1 h2

movesAvailable :: PillControlState -> [Move]
movesAvailable (PillControlState
	PillControlStateEq { parity = par }
	PillControlStateHysteresis { forbidden = m, flexibility = x }) =
	[ Move d r
	| d <- [Just down  | checking]
	    ++ [Nothing    | not checking] -- don't have a seizure
	    ++ [Just left  | direction m /= Just left ]
	    ++ [Just right | direction m /= Just right]
	    ++ [Nothing    | checking]
	, r <- [Nothing | not inflexible]
	    ++ [Just Clockwise]
	    ++ [Nothing | inflexible]
	]
	where
	checking = par == Checking
	inflexible = x == Inflexible

{-# INLINE even' #-}
even' :: Int -> Bool
even' n = 0 == n .&. 1

-- | The 'Int' is the pill speed.
--
-- This function "takes your word for it" on the 'Move' argument; it does not
-- check whether:
--
-- * the given 'Move' conflicts with the 'forbidden' field of the
--   'PillControlState' or
-- * the 'direction' of the 'Move' conflicts with the 'parity' of the
--   'PillControlState'.
--
-- ...but see also 'movesAvailable', which does do these checks.
advance :: Board -> Int -> PillControlState -> Move -> Maybe PillControlState
advance b d (PillControlState e h) m = guard success $> PillControlState
	PillControlStateEq
		{ pill = pill4
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
		, flexibility = case (flexibility h, rotation m) of
			(Flexible, _) -> Flexible
			(Pending, Nothing) -> Flexible
			(Inflexible, Just _) -> Pending
			_ -> Inflexible
		}
	where
	-- The order of operations here was determined experimentally (i.e. not by
	-- reading the assembly).
	pill0 = pill e
	(motion1, pill1) = try move naturalDrop pill0
	(motion2, pill2) = try move (direction m) pill1
	(motion3, pill3) = try rotate (rotation m) pill2
	-- Famous-ish bug: pressing rotate and left on the same frame when the pill
	-- is vertical gets you a bonus left move.
	(motion4, pill4) = case (orientation (content (pill e)), m, move b pill3 left) of
		(Vertical, Move (Just dir) (Just _), Just p') | dir == left -> (True, p')
		_ -> (False, pill3)

	success = and [motion1, motion2 || motion4, motion3]
	try f mx p = case mx of
		Nothing -> (True, p)
		Just x -> case f b p x of
			Nothing -> (False, p )
			Just p' -> (True , p')

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
-- pairs whose key is greater than the one currently being inserted. Returns
-- 'Nothing' if an equal or lesser key is already in the map.
insertMinMap :: POrd k => k -> v -> MinMap k v -> Maybe (MinMap k v)
insertMinMap k v m = case compareKeys (pcompare k . fst) m of
	HasGtOrEq -> Nothing
	HasLt -> Just $ (k,v):filter ((In==) . pcompare k . fst) m
	OnlyIn -> Just $ (k,v):m

type Visited = HashMap PillControlStateEq (MinMap PillControlStateHysteresis [Move])

-- | Mark a particular 'PillControlState' as visited. Returns 'Nothing' if it
-- was already so marked.
insertVisited :: PillControlStateEq -> PillControlStateHysteresis -> [Move] -> Visited -> Maybe Visited
-- TODO: use alterF instead maybe
insertVisited e h ms v = case HM.lookup e v of
	Just mm -> (\mm' -> HM.insert e mm' v) <$> insertMinMap h ms mm
	Nothing -> Just (HM.insert e [(h,ms)] v)

-- TODO: provide a way to cache results across pill drops
reachable :: Board -> Int -> Pill -> Parity -> HashMap Pill [Move]
reachable b dropSpeed initPill initParity = summarize (execState (dfs [] initPCS) HM.empty) where
	initPCS = PillControlState
		PillControlStateEq
			{ pill = initPill { content = (content initPill) { bottomLeftColor = Yellow, otherColor = Yellow } }
			, parity = initParity
			, forcedDropParity = if even' dropSpeed then initParity else notParity initParity
			}
		PillControlStateHysteresis
			{ forbidden = Move Nothing Nothing
			, cost = 0
			, forcedDrop = dropSpeed
			, flexibility = Inflexible
			}

	dfs ms s@(PillControlState e h) = do
		mvisited <- gets (insertVisited e h ms)
		for_ mvisited $ \visited -> do
			put visited
			for_ (movesAvailable s) $ \m -> do
				for_ (advance b dropSpeed s m) (dfs (m:ms))

	summarize = id
		. fmap (reverse . snd)
		. HM.foldlWithKey' summarize' HM.empty

	summarize' summary e mm = case move b (pill e) down of
		Nothing -> foldl' (summarize'' (pill e)) summary mm
		Just _ -> summary

	summarize'' p summary (h, ms) = foldl'
		(\summary' (p', ms') -> HM.insertWith keepMinCost p' (cost h, ms') summary')
		summary
		(expandMoves initPill p ms)

	keepMinCost v1@(c1, _) v2@(c2, _) = if c1 < c2 then v1 else v2

notRotation :: Rotation -> Rotation
notRotation Clockwise = Counterclockwise
notRotation Counterclockwise = Clockwise

-- | Internal only. Alternate rotations, starting with 'Clockwise', so that
-- even if there are rotations on consecutive frames, it can be mapped to a
-- valid sequence of button presses.
alternateRotations :: [Move] -> [Move]
alternateRotations = go Clockwise where
	go rot (Move dir Just{}:ms) = Move dir (Just rot) : go (notRotation rot) ms
	go rot (m:ms) = m : go rot ms
	go rot [] = []

-- | Internal only. Take a move sequence from 'alternateRotations' and, if
-- possible, produce a modified one that has 180-degree different total
-- rotation. Returns a list of length 1 if that is possible; an empty list
-- otherwise.
aboutFace :: [Move] -> [[Move]]
aboutFace = goInflexible id id where
	goInflexible same swapped (m@(Move dir (Just rot)):ms) = goPending (same.(m:)) (swapped.(Move dir (Just (notRotation rot)):)) ms
	goInflexible same swapped (m:ms) = goInflexible (same.(m:)) (same.(m:)) ms
	goInflexible same swapped [] = []

	goPending same swapped (m@(Move dir (Just rot)):ms) = goInflexible (same.(m:)) (swapped.(Move dir (Just (notRotation rot)):)) ms
	goPending same swapped ms = [swapped ms]

-- | Internal only. Given an original pill and the result of a reachability
-- search, fix up the rotations in the result move sequence and the colors in
-- the result pill.
expandMoves :: Pill -> Pill -> [Move] -> [(Pill, [Move])]
expandMoves original colorless ms = zip [final, finalSwapped] mss where
	mss = fixedMoves : aboutFace fixedMoves
	fixedMoves = alternateRotations ms

	final = colorless { content = finalContent }
	finalSwapped = final { content = (content final)
		{ bottomLeftColor = otherColor (content final)
		, otherColor = bottomLeftColor (content final)
		}}
	finalContent = if orientation (content colorless) == orientation (content original)
		then content original
		else rotateContent (content original) Clockwise

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
	pp (Move dir rot) = maybe "·" pp dir ++ maybe " " pp rot
instance PP [Move] where pp ms = show (length ms) ++ "/" ++ (ms >>= pp)
instance PP RotationFlexibility where
	pp Inflexible = "·"
	pp Pending = "?"
	pp Flexible = "⤡"
instance PP PillControlStateHysteresis where
	pp (PillControlStateHysteresis m c d x) = unwords [pp m, show c, show d, pp x]
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
