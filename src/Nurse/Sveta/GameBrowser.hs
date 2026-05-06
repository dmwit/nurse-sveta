module Nurse.Sveta.GameBrowser where

import Nurse.Sveta.Util
import Nurse.Sveta.Widget

import qualified Control.Monad.State as State
import qualified Data.IntMap as IM
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

type IMForest a = IntMap (IMTree a)
data IMTree a = IMTree
	{ tLabel :: a
	-- If we were to use a list or sequence, then paths constructed from a list
	-- of indices would need to be updated when we deleted a node. So instead,
	-- we use an IntMap as a model for the list. The keys are arbitrary, except
	-- that we consider smaller keys to come earlier in the sequence. Deleting
	-- a node from the sequence then just means deleting the relevant key/value
	-- pair, which leaves all the other keys unchanged (and therefore paths
	-- that mention them need not be updated).
	--
	-- This is only notionally a list, though. Concretely we allow the keys to
	-- leak out of the interface, e.g. the Eq interface considers them. Since
	-- we want to work with paths that are keys, there's not really much
	-- choice except to expose them.
	, tChildren :: IMForest a
	} deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance PP a => PP (IMTree a) where pp = pp1
instance PP1 IMTree where
	liftPP1 ppElem t = if null (tChildren t)
		then ppElem (tLabel t)
		else printf "%s => %s"
			(ppElem (tLabel t))
			(liftPP1Forest ppElem (tChildren t))

instance Default a => Default (IMTree a) where def = IMTree def def

ppForest :: PP a => IMForest a -> String
ppForest = liftPP1Forest pp

liftPP1Forest :: (a -> String) -> IMForest a -> String
liftPP1Forest ppElem = liftPP1 (liftPP1 ppElem) . IM.elems

-- | Add a variation, unless there's already one that matches according to some
-- summarization. Returns the index of a match (which is the newly inserted
-- element if nothing in the input matches).
fInsertOn :: Eq b => (a -> b) -> a -> IMForest a -> (Int, IMForest a)
fInsertOn f a ts = case IM.toAscList (IM.filter (\t' -> f a == f (tLabel t')) ts) of
	(k, _):_ -> (k, ts)
	_ -> (k, IM.insert k (IMTree a def) ts) where
		k = maybe 0 ((1+) . fst) (IM.lookupMax ts)

data GameStateEdit
	= GenerateLevel Word16 Int
	| Lock Lookahead MidPlacement
	deriving (Eq, Ord, Read, Show)

instance PP GameStateEdit where
	pp = \case
		GenerateLevel seed level -> printf "%2d:%04X" level seed
		Lock lk mp -> printf "%s@%s" (pp lk) (pp mp)

data GameState = GameState
	{ board :: Board
	, pillSequence :: Vector Lookahead
	, pillIndex :: Int
	} deriving (Eq, Ord, Read, Show)

instance Default GameState where
	def = GameState (emptyBoard 8 16) V.empty 0

instance PP GameState where
	pp gs = case length (pillSequence gs) of
		0 -> "\n" ++ pp (board gs)
		_ -> printf "\n%s%s >%s< %s\n"
			(pp (board gs))
			(ppLookaheads . V.take n . pillSequence $ gs)
			(pp (pillSequence gs V.! n))
			(ppLookaheads . V.drop (n+1) . pillSequence $ gs)
			where
			ppLookaheads = unwords . map pp . toList
			n = pillIndex gs `mod` length (pillSequence gs)

applyEdit :: GameState -> GameStateEdit -> Maybe GameState
applyEdit s = \case
	GenerateLevel seed level -> let (lks, b) = randomLevel seed level in pure GameState
		{ board = b
		, pillSequence = lks
		, pillIndex = 0
		}
	Lock lk mp -> place (board s) (mpPill mp lk) <&> \results -> s
		{ board = snd results
		, pillIndex = if nextIndex >= len then nextIndex - len else nextIndex
		} where
		nextIndex = pillIndex s + 1
		len = V.length (pillSequence s)

data UILabel = UILabel
	{ parentEdge :: GameStateEdit
	, stateCache :: GameState
	, activeChild :: Maybe Int
	} deriving (Eq, Ord, Read, Show)

instance PP UILabel where
	pp lbl = printf "{ δ = %s, child = %s }"
		(pp (parentEdge lbl))
		(maybe "ε" pp (activeChild lbl))

-- | I want to just store a 'UITree' with an empty game state, say, 'def', as
-- the root state. Unfortunately, there's no good thing to put in the
-- 'GameStateEdit' at the root. So instead, there is a phantom root node with
-- that state implicitly, and we store the children and active variation that
-- are usually in the 'UITree' directly in the 'UIModel' instead. The
-- 'selectionDepth' is with reference to the phantom tree, so depth 0 means the
-- implicit root.
data UIModel = UIModel
	{ nodes :: UIForest
	, activeRoot :: Maybe Int
	, selectionDepth :: Int
	} deriving (Eq, Ord, Read, Show)

instance Default UIModel where def = UIModel def def def
instance PP UIModel where
	pp ui = printf
		"ui { depth = %d, root = %s, nodes = %s }"
		(selectionDepth ui)
		(maybe "ε" pp (activeRoot ui))
		(ppForest (nodes ui))

type UITree = IMTree UILabel
type UIForest = IMForest UILabel
type Variation = [Int]

descendL :: Monad m => (UIModel -> MaybeT m Int) -> UIModel -> MaybeT m (GameState, UIModel, UIModel -> UIModel)
descendL choose = go def where
	go s ui = lift (runMaybeT (choose ui)) >>= \case
		Nothing -> pure (s, ui, id)
		Just i -> do
			t <- MaybeT . pure $ nodes ui IM.!? i
			(s', ui', rebuild) <- go (stateCache (tLabel t)) UIModel
				{ nodes = tChildren t
				, activeRoot = activeChild (tLabel t)
				, selectionDepth = selectionDepth ui - 1
				}
			pure (s', ui', \ui'' -> let
				rebuilt = rebuild ui''
				t' = IMTree
					{ tLabel = (tLabel t) { activeChild = activeRoot rebuilt }
					, tChildren = nodes rebuilt
					}
				in ui
					{ nodes = IM.insert i t' (nodes ui)
					, selectionDepth = selectionDepth rebuilt + 1
					})

variationL :: Variation -> UIModel -> Maybe (GameState, UIModel, UIModel -> UIModel)
variationL var = flip State.evalState var . runMaybeT . descendL \_ui -> do
	i:is <- State.get
	i <$ State.put is

-- | If you get a 'Just', its 'selectionDepth' is guaranteed to be 0.
selectedUIL :: UIModel -> Maybe (GameState, UIModel, UIModel -> UIModel)
selectedUIL = join . runMaybeT . descendL \ui -> do
	guard (selectionDepth ui > 0)
	(MaybeT . pure . activeRoot) ui

selectedPSM :: UIModel -> PlayerStateModel
selectedPSM ui = PSM
	{ psmBoard = board gs
	, psmLookahead = pillSequence gs V.!? pillIndex gs
	, psmOverlay = []
	} where
	gs = defOr (frst <$> selectedUIL ui)

activeVariation :: HasCallStack => UIModel -> Variation
activeVariation = unfoldr \ui -> do
	i <- activeRoot ui
	t <- nodes ui IM.!? i
	pure $ (,) i UIModel
		{ nodes = tChildren t
		, activeRoot = activeChild (tLabel t)
		, selectionDepth = error "activeVariation is not supposed to read the selection depth, but it did"
		}

activateVariation :: Variation -> UIModel -> Maybe UIModel
activateVariation [] ui = Just ui
activateVariation (i:is) ui = do
	t <- nodes ui IM.!? i
	t' <- tActivateVariation is t
	pure ui
		{ nodes = IM.insert i t' (nodes ui)
		, activeRoot = Just i
		}

tActivateVariation :: Variation -> UITree -> Maybe UITree
tActivateVariation [] t = Just t
tActivateVariation (i:is) t = do
	t' <- tChildren t IM.!? i
	t'' <- tActivateVariation is t'
	pure t
		{ tChildren = IM.insert i t'' (tChildren t)
		, tLabel = (tLabel t) { activeChild = Just i }
		}

selectVariation :: Variation -> UIModel -> Maybe UIModel
selectVariation is ui = activateVariation is ui { selectionDepth = length is }

unSnoc :: [a] -> Maybe (a, [a])
unSnoc [] = Nothing
unSnoc (x:xs) = Just (go x xs) where
	go x = \case
		[] -> (x, [])
		x':xs' -> (x:) <$> go x' xs'

deleteVariation :: Variation -> UIModel -> Maybe UIModel
deleteVariation is ui = do
	(i, is') <- unSnoc is
	(_, ui', rebuild) <- variationL is' ui
	let nodes' = IM.delete i (nodes ui')
	    active' = case IM.toList nodes' of
	    	[(i', _)] -> Just i'
	    	_ -> activeRoot ui' >>= ensure (i /=)
	pure . truncateSelectionDepth . rebuild $ ui'
		{ nodes = IM.delete i (nodes ui')
		, activeRoot = active'
		}

truncateSelectionDepth :: UIModel -> UIModel
truncateSelectionDepth ui = setSelectionDepth ui (min (length (activeVariation ui)) (selectionDepth ui))

-- | Does not do any sanity checking, just directly sets the depth.
setSelectionDepth :: UIModel -> Int -> UIModel
setSelectionDepth ui d = ui { selectionDepth = d }

forward :: UIModel -> Maybe UIModel
forward ui = setSelectionDepth ui <$> ensure (< length (activeVariation ui)) (selectionDepth ui + 1)

backward :: UIModel -> Maybe UIModel
backward ui = setSelectionDepth ui <$> ensure (>= 0) (selectionDepth ui - 1)

advance :: GameStateEdit -> UIModel -> Maybe UIModel
advance e ui = do
	(s, ui', rebuild) <- selectedUIL ui
	s' <- applyEdit s e
	let lbl = UILabel
	    	{ parentEdge = e
	    	, stateCache = s'
	    	, activeChild = Nothing
	    	}
	    -- Any pill placement can be modified to have the same effect by
	    -- mirroring the lookahead and rotating clockwise two additional times.
	    -- This leads to some oddities in the UI. Should those really lead to
	    -- different variations in the tree viewer? Options include:
	    --
	    -- 1. Keep them as separate variations. Then there are two possible
	    --    next game states that are the same but are considered different,
	    --    which seems odd.
	    -- 2. Merge the variations. Then there's a question of what
	    --    activeLookahead should do. It looks at the lookahead that
	    --    leads to the next node in the active variation. If the user
	    --    has arrived at that node via two different lookaheads in the
	    --    past, it has to pick one.
	    --
	    -- We opt for (2), which is encoded in the definition of nodes', with
	    -- the stance that the active lookahead is the one the user used most
	    -- recently, which is encoded in the definition of nodes''.
	    (i, nodes') = fInsertOn normalizedEdit lbl (nodes ui')
	    nodes'' = IM.adjust (\t -> t { tLabel = (tLabel t) { parentEdge = e } }) i nodes'
	pure $ rebuild UIModel
		{ nodes = nodes''
		, activeRoot = Just i
		, selectionDepth = 1
		}

-- | Returns 'parentEdge', but all lookaheads have had their colors put in a
-- standard order.
normalizedEdit :: UILabel -> GameStateEdit
normalizedEdit lbl = case parentEdge lbl of
	e@GenerateLevel{} -> e
	Lock lk mp -> Lock (Lookahead lo hi) mp where
		[lo, hi] = sort [leftColor lk, rightColor lk]

activeLookahead :: UIModel -> Maybe Lookahead
activeLookahead ui = do
	(_, ui', _) <- selectedUIL ui
	i <- activeRoot ui'
	t <- nodes ui' IM.!? i
	case parentEdge (tLabel t) of
		Lock lk _ -> Just lk
		_ -> Nothing

seedLookahead :: UIModel -> Maybe Lookahead
seedLookahead ui = do
	(s, _, _) <- selectedUIL ui
	pillSequence s V.!? pillIndex s
