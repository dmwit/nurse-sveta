module Nurse.Sveta.GameBrowser where

import Nurse.Sveta.Util
import Nurse.Sveta.Widget

import qualified Data.IntMap as IM
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

data MoveTree m = MoveTree
	{ mainSequence :: Seq m
	, variations :: Seq (MoveTree m)
	} deriving (Eq, Ord, Read, Show, Functor)

instance Default (MoveTree m) where def = MoveTree def def

indexVariations_ :: HasCallStack => MoveTree m -> Seq Int -> MoveTree m
indexVariations_ t is = fromJust $ indexVariations t is

indexVariations :: MoveTree m -> Seq Int -> Maybe (MoveTree m)
indexVariations t = flip foldl' (Just t) \mt i -> do
	t' <- mt
	variations t' Seq.!? i

indexVariationsL :: MoveTree m -> Seq Int -> Maybe (MoveTree m, MoveTree m -> MoveTree m)
indexVariationsL = go id where
	go rebuild mt = \case
		Seq.Empty -> Just (mt, rebuild)
		i Seq.:<| is -> do
			(b, h Seq.:<| e) <- Just $ Seq.splitAt i (variations mt)
			go (\h' -> mt { variations = b <> Seq.singleton h' <> e }) h is

-- | Cut the main sequence at the given point, and make the rest of the
-- sequence into a variation. 'Nothing' indicates an attempt to cut past the
-- end of the main sequence.
splitMainSequence :: Int -> MoveTree m -> Maybe (MoveTree m)
splitMainSequence n mt = case compare n (length ms) of
	LT -> Just MoveTree
		{ mainSequence = b
		, variations = Seq.singleton MoveTree
			{ mainSequence = e
			, variations = variations mt
			}
		}
	EQ -> Just mt
	GT -> Nothing
	where
	ms = mainSequence mt
	(b, e) = Seq.splitAt n ms

-- | Add a variation, if it doesn't already exist. Returns the index you can
-- find the given variation at after the addition.
insertVariation :: (Eq m, Show m, HasCallStack) => m -> MoveTree m -> (MoveTree m, Int)
insertVariation m mt = len `seq` flip (,) len case e of
	_ Seq.:<| _ -> mt
	_ -> mt
		{ variations = variations mt Seq.:|> MoveTree
			{ mainSequence = Seq.singleton m
			, variations = def
			}
		}
	where
	(b, e) = Seq.breakl match (variations mt)
	len = length b
	match mt' = case mainSequence mt' of
		m' Seq.:<| _ -> m == m'
		_ -> error $ "insertVariation (" ++ show m ++ ") (" ++ show mt ++ ")"

data InsertionResult
	= AlreadyInMainSequence -- ^ The move was already at the requested spot in the main sequence.
	| AlreadyInVariation Int -- ^ The move was already at the requested spot, and can be found in the variation given in the 'Int'.
	| Appended -- ^ A new move has been added at the end of the current main sequence.
	| SplitAndInserted -- ^ A new level of variations has been added to the tree, and your newly inserted element is in variation @1@.
	| Inserted Int -- ^ A new variation has been added at the current level of the tree, and your newly inserted element is in the variation given in the 'Int'.
	deriving (Eq, Ord, Read, Show)

-- | If the tree doesn't have the given move at the given index already, cut
-- the main sequence at that point and add the move as a variation. 'Nothing'
-- indicates that we must choose a variation before we get to the given index.
splitAndInsertVariation :: (Eq m, Show m, HasCallStack) => Int -> m -> MoveTree m -> Maybe (MoveTree m, InsertionResult)
splitAndInsertVariation n m mt = splitMainSequence n mt <&> \mt' -> case insertVariation m mt' of
	result@(mt'', i)
		| 0 == length (variations mt') -> (mt { mainSequence = mainSequence mt Seq.:|> m }, Appended)
		| i == length (variations mt') -> (,) mt'' if n == length (mainSequence mt)
			then Inserted i
			else SplitAndInserted
		| otherwise -> (,) mt if n == length (mainSequence mt)
			then AlreadyInVariation i
			else AlreadyInMainSequence

data GameStateEdit
	= GenerateLevel Word16 Int
	| Lock Pill
	deriving (Eq, Ord, Read, Show)

data GameState = GameState
	{ board :: Board
	, pillSequence :: Vector Lookahead
	, pillIndex :: Int
	} deriving (Eq, Ord, Read, Show)

instance Default GameState where
	def = GameState (emptyBoard 8 16) V.empty 0

applyEdit :: GameState -> GameStateEdit -> GameState
applyEdit s = \case
	GenerateLevel seed level -> let (lks, b) = randomLevel seed level in GameState
		{ board = b
		, pillSequence = lks
		, pillIndex = 0
		}
	Lock p -> s
		{ board = maybe (board s) snd (place (board s) p)
		, pillIndex = if nextIndex >= len then nextIndex - len else nextIndex
		} where
		nextIndex = pillIndex s + 1
		len = V.length (pillSequence s)

-- eventually we may want some sort of mildly intelligent cache, but for now let's just make all the states
applyEdits :: MoveTree GameStateEdit -> MoveTree (GameStateEdit, GameState)
applyEdits = go def where
	go s mt = let (sNext, ss) = mapAccumL accumulate s (mainSequence mt) in MoveTree
		{ mainSequence = ss
		, variations = go sNext <$> variations mt
		}
	accumulate s e = let s' = applyEdit s e in (s', (e, s'))

data ActiveVariations = ActiveVariations
	{ activeHere :: Int
	, activeChildren :: IntMap ActiveVariations
	} deriving (Eq, Ord, Read, Show)

-- | Prefers the left/first argument.
instance Semigroup ActiveVariations where
	ActiveVariations i itree <> ActiveVariations _i' itree' = ActiveVariations i (IM.unionWith (<>) itree itree')

singletonVariations :: Int -> ActiveVariations
singletonVariations i = ActiveVariations
	{ activeHere = i
	, activeChildren = def
	}

fromVariations :: Seq Int -> Maybe ActiveVariations
fromVariations = \case
	Seq.Empty -> Nothing
	is Seq.:|> i -> Just $ foldr (\i' -> ActiveVariations i' . IM.singleton i') (singletonVariations i) is

activeChild :: ActiveVariations -> Maybe ActiveVariations
activeChild av = activeChildren av IM.!? activeHere av

activateVariation :: Seq Int -> Maybe ActiveVariations -> Maybe ActiveVariations
activateVariation is = (fromVariations is <>)

atDepth :: Int -> (ActiveVariations -> ActiveVariations) -> Maybe ActiveVariations -> Maybe ActiveVariations
atDepth d0 f = Just . maybeOn0 d0 where
	maybeOn0 d = go d . fromMaybe (singletonVariations 0)
	go 0 av = f av
	go d av = av
		{ activeChildren = IM.insert
			(activeHere av)
			(maybeOn0 (d-1) (activeChild av))
			(activeChildren av)
		}

extendVariationAtDepth :: Int -> Int -> Maybe ActiveVariations -> Maybe ActiveVariations
extendVariationAtDepth d i = atDepth d (singletonVariations i <>)

splitVariationAtDepth :: Int -> Maybe ActiveVariations -> Maybe ActiveVariations
splitVariationAtDepth d = Just . go d where
	go 0 Nothing = singletonVariations 1
	go 0 (Just av) = ActiveVariations
		{ activeHere = 1
		, activeChildren = IM.singleton 0 av
		}
	go n Nothing = ActiveVariations
		{ activeHere = 0
		, activeChildren = IM.singleton 0 (go (n-1) Nothing)
		}
	go n (Just av) = av
		{ activeChildren = IM.insert i (go (n-1) child) (activeChildren av)
		}
		where
		i = activeHere av
		child = activeChildren av IM.!? i

ensureDepth :: Int -> Maybe ActiveVariations -> Maybe ActiveVariations
ensureDepth n = (<> fromVariations (Seq.replicate n 0))

activePath :: Maybe ActiveVariations -> [Int]
activePath = foldMap \av -> activeHere av : activePath (activeChild av)

-- | *Always* returns a sequence of the supplied length
activePathOfDepth :: Maybe ActiveVariations -> Int -> Seq Int
activePathOfDepth mav n = prefix <> Seq.replicate (n - length prefix) 0 where
	path = activePath mav
	prefix = Seq.fromList (take n path)

-- ^ The 'Default' instance gives a canonical "one before the beginning" node.
data MoveSelection = MoveSelection
	{ mainSequenceIndex :: Int
	, variationDepth :: Int
	} deriving (Eq, Ord, Read, Show)

instance Default MoveSelection where
	def = MoveSelection
		{ mainSequenceIndex = -1
		, variationDepth = 0
		}

modifyMainSequenceIndex :: Int -> MoveSelection -> MoveSelection
modifyMainSequenceIndex di sel = sel { mainSequenceIndex = mainSequenceIndex sel + di }

data UIModel = UIModel
	{ nodes :: MoveTree (GameStateEdit, GameState)
	, activeVariations :: Maybe ActiveVariations
	, moveSelection :: MoveSelection
	} deriving (Eq, Ord, Read, Show)

instance Default UIModel where def = UIModel def def def

uiVariationDepth :: UIModel -> Int
uiVariationDepth = variationDepth . moveSelection

uiMainSequenceIndex :: UIModel -> Int
uiMainSequenceIndex = mainSequenceIndex . moveSelection

uiCurrentPSM :: UIModel -> PlayerStateModel
uiCurrentPSM ui = PSM
	{ psmBoard = board gs
	, psmLookahead = lookahead
	, psmOverlay = []
	} where
	gs = uiCurrentState ui
	lookahead = (pillSequence gs V.!?) (pillIndex gs)

-- | doesn't check if the move selection is valid for the current active variations and nodes
setSelection :: UIModel -> MoveSelection -> UIModel
setSelection ui sel = ui { moveSelection = sel }

activeTrees :: UIModel -> [MoveTree (GameStateEdit, GameState)]
activeTrees ui = go (nodes ui) (activePath (activeVariations ui)) where
	go mt is = mt : case is of
		[] -> []
		h:t -> go (variations mt `Seq.index` h) t

-- | like 'activeTrees', but also returns 0-variations past the end of the activity
defaultTrees :: UIModel -> [MoveTree (GameStateEdit, GameState)]
defaultTrees ui = go (nodes ui) (activePath (activeVariations ui)) where
	go mt is = mt : case (is, variations mt) of
		([], Seq.Empty) -> []
		([], mt' Seq.:<| _) -> go mt' []
		(i:is, _) -> go (variations mt `Seq.index` i) is

uiModifyVariation :: (Maybe ActiveVariations -> Maybe ActiveVariations) -> UIModel -> UIModel
uiModifyVariation f ui = ui { activeVariations = f (activeVariations ui) }

uiEnsureDepth :: UIModel -> UIModel
uiEnsureDepth ui = uiModifyVariation (ensureDepth (uiVariationDepth ui)) ui

uiActivePath :: UIModel -> Seq Int
uiActivePath ui = activePathOfDepth (activeVariations ui) (uiVariationDepth ui)

uiActivateVariation :: Seq Int -> UIModel -> UIModel
uiActivateVariation = uiModifyVariation . activateVariation

uiExtendVariation :: Int -> UIModel -> UIModel
uiExtendVariation i ui = uiModifyVariation (extendVariationAtDepth (uiVariationDepth ui) i) ui

uiSplitVariation :: UIModel -> UIModel
uiSplitVariation ui = uiModifyVariation (splitVariationAtDepth (uiVariationDepth ui)) ui

uiFocusedTree :: HasCallStack => UIModel -> MoveTree (GameStateEdit, GameState)
uiFocusedTree ui = indexVariations_ (nodes ui) (uiActivePath ui)

uiCurrentState :: UIModel -> GameState
uiCurrentState ui = defOr . fmap snd $ mainSequence (uiFocusedTree ui) Seq.!? uiMainSequenceIndex ui

uiIsLegalEdit :: UIModel -> GameStateEdit -> Bool
uiIsLegalEdit ui = isLegalEdit (uiCurrentState ui)

isLegalEdit :: GameState -> GameStateEdit -> Bool
isLegalEdit _ GenerateLevel{} = True
isLegalEdit gs (Lock pill) = isJust (place (board gs) pill)

normalizeLarge :: HasCallStack => UIModel -> MoveSelection -> Maybe MoveSelection
normalizeLarge ui sel0 = go (drop (variationDepth sel0) (defaultTrees ui)) sel0 where
	go [] _ = Nothing
	go (mt:mts) sel@(MoveSelection { mainSequenceIndex = i, variationDepth = d })
		| i < len = Just sel
		| otherwise = go mts MoveSelection { mainSequenceIndex = i - len, variationDepth = d + 1 }
		where len = length (mainSequence mt)

normalizeSmall :: HasCallStack => UIModel -> MoveSelection -> Maybe MoveSelection
normalizeSmall ui sel0 = go ns0 sel0 where
	ns0_ = take (variationDepth sel0) (activeTrees ui)
	ns0 | length ns0_ == variationDepth sel0 = reverse ns0_
	    | otherwise = error $ "normalizeSmall (" ++ show ui ++ ") (" ++ show sel0 ++ ")"

	go ns sel@(MoveSelection { mainSequenceIndex = i, variationDepth = d }) = case compare d 0 of
		LT -> Nothing
		EQ -> sel <$ guard (i >= -1)
		GT | i < 0 -> case ns of
			[] -> error $ "normalizeSmall (" ++ show ui ++ ") (" ++ show sel0 ++ ")"
			n:nt -> go nt MoveSelection { mainSequenceIndex = length (variations n) + i, variationDepth = d - 1 }
		_ -> Just sel

uiModifySequenceIndex :: Int -> UIModel -> Maybe UIModel
uiModifySequenceIndex di ui = id
	. fmap (normalizeActive . setSelection ui)
	. normalizeSelection ui
	. modifyMainSequenceIndex di
	$ moveSelection ui
	where
	(normalizeSelection, normalizeActive) = case compare di 0 of
		LT -> (normalizeSmall, id)
		EQ -> (const Just, id)
		GT -> (normalizeLarge, uiEnsureDepth)

uiForward :: UIModel -> Maybe UIModel
uiForward = uiModifySequenceIndex 1

uiBackward :: UIModel -> Maybe UIModel
uiBackward = uiModifySequenceIndex (-1)

normalizeActiveVariations :: MoveTree m -> Maybe ActiveVariations -> Maybe ActiveVariations
normalizeActiveVariations mt (Just av)
	| varCount <= 0 = Nothing
	| otherwise = Just ActiveVariations
		{ activeHere = i
		, activeChildren = IM.fromList do
			ix <- [0..varCount-1]
			let child_ = normalizeActiveVariations (Seq.index vars ix) (activeChildren av IM.!? ix)
			maybe [] (\child -> [(ix, child)]) child_
		}
	where
	vars = variations mt
	varCount = length vars
	i = min (varCount-1) (max 0 (activeHere av))
normalizeActiveVariations _ Nothing = Nothing

uiNormalizeActiveVariations :: UIModel -> UIModel
uiNormalizeActiveVariations ui = ui
	{ activeVariations = normalizeActiveVariations (nodes ui) (activeVariations ui)
	}

data MoveTreeAddress = MoveTreeAddress
	{ mtaVariations :: Seq Int
	, mtaMainSequenceIndex :: Int
	} deriving (Eq, Ord, Read, Show)

instance Default MoveTreeAddress where def = MoveTreeAddress def (-1)

uiVisitAddress :: UIModel -> MoveTreeAddress -> Maybe UIModel
uiVisitAddress ui addr = do
	_ <- indexVariations (nodes ui) (mtaVariations addr)
	let sel = MoveSelection
	    	{ variationDepth = length (mtaVariations addr)
	    	, mainSequenceIndex = mtaMainSequenceIndex addr
	    	}
	let ui' = uiEnsureDepth (uiActivateVariation (mtaVariations addr) ui)
	sel' <- normalizeSmall ui' =<< normalizeLarge ui' sel
	pure . uiNormalizeActiveVariations $ setSelection ui' sel'

uiDeleteCurrent :: HasCallStack => UIModel -> Maybe UIModel
uiDeleteCurrent ui = do
	let i = uiMainSequenceIndex ui
	guard (i >= 0)
	(focusedTree, rebuildTree) <- indexVariationsL (nodes ui) (uiActivePath ui)
	guard (i < length (mainSequence focusedTree))
	let (b, _ Seq.:<| e) = Seq.splitAt i (mainSequence focusedTree)
	    focusedTree' = focusedTree { mainSequence = b <> e }
	    ui' = uiNormalizeActiveVariations ui
	    	{ nodes = rebuildTree focusedTree'
	    	, moveSelection = (moveSelection ui)
	    		{ mainSequenceIndex = i - 1
	    		}
	    	}
	normalizeSmall ui' (moveSelection ui') <&> setSelection ui'

uiTryAdvance :: HasCallStack => GameStateEdit -> UIModel -> Maybe UIModel
uiTryAdvance e ui = uiAdvance e ui <$ guard (uiIsLegalEdit ui e)

-- TODO: Check that extendVariation and splitVariation do what you think they
-- do. In particular, are they adding an extra leaf variation of 0 in some/all
-- cases?
uiAdvance :: HasCallStack => GameStateEdit -> UIModel -> UIModel
uiAdvance e ui = fromMaybe uiError do
	(focusedTree, rebuildTree) <- indexVariationsL (nodes ui) (uiActivePath ui)
	let focusedState = defOr . fmap snd $ mainSequence focusedTree Seq.!? uiMainSequenceIndex ui
	(focusedTree', action) <- splitAndInsertVariation (uiMainSequenceIndex ui + 1) (e, applyEdit focusedState e) focusedTree
	let ui' = ui { nodes = rebuildTree focusedTree' }
	uiForward case action of
		AlreadyInMainSequence -> ui'
		AlreadyInVariation i -> uiExtendVariation i ui'
		Appended -> ui'
		SplitAndInserted -> uiSplitVariation ui'
		Inserted i -> uiExtendVariation i ui'
	where
	uiError = error $ "uiAdvance (" ++ show e ++ ") (" ++ show ui ++ ")"
