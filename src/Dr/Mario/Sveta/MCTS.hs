module Dr.Mario.Sveta.MCTS (
	MCTree(..),
	tChildren,
	MCNode(..),
	MCEdge(..),
	MCTSParameters(..),
	emptyTree,
	mcts,
	ucb1,
	emptyPreprocessor,
	neverSummarize,
	) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Dr.Mario.Sveta.Cache (Cache, Shared(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Dr.Mario.Sveta.Cache as Cache

-- | A representation of the current state of a run of Monte Carlo tree search.
-- An invariant expected (and preserved) by the code in this module is that if
-- @'Shared' summary@ appears somewhere in this structure, then @summary@ is a
-- key in 'tCache'.
data MCTree stats move summary = MCTree
	{ tCache :: Cache summary (MCNode stats move summary)
	, tRoot :: MCNode stats move summary
	} deriving (Eq, Show)

-- | A node in the Monte Carlo search tree. An invariant expected (and
-- preserved) by the code in this module is that either @'nStatistics' =
-- foldMap 'eStatistics' . 'nChildren'@ or 'nChildren' is empty.
data MCNode stats move summary = MCNode
	{ nStatistics :: stats
	, nChildren :: HashMap move (MCEdge stats move summary)
	, nUnexplored :: [move]
	} deriving (Eq, Show)

-- | An edge in the Monte Carlo search tree. Unlike for 'MCNode's, we do not
-- expect there to be any specific relationship between the statistics stored
-- on an edge and the stastics stored at the target of the edge. This is
-- because its target may be updated through another path through the search
-- tree, so it may have more accurate statistics than what are available just
-- through this edge.
data MCEdge stats move summary = MCEdge
	{ eStatistics :: stats
	, eTarget :: Shared summary (MCNode stats move summary)
	} deriving (Eq, Show)

-- | Assumes that all shared targets are actually available in the cache. (This
-- invariant is preserved by the functions in this module.)
tChildren :: (Eq summary, Hashable summary) => MCTree stats move summary -> HashMap move (MCNode stats move summary)
tChildren MCTree { tCache = cache, tRoot = n } = (cache Cache.!) . eTarget <$> nChildren n

data MCTSParameters m stats score move position summary player = MCTSParameters
	{ score :: player -> stats -> stats -> score
	-- ^ Given which player is currently choosing a move and the statistics for
	-- a position's node and a move's edge out of that node, compute a priority
	-- score for searching this node. Larger scores are searched first.
	--
	-- See also 'ucb1'.
	, evaluate :: position -> m stats
	-- ^ Given a leaf position, compute some statistics. These statistics
	-- typically include the utilities for each player and a count of times
	-- visited (which this function should initialize to 1). The statistics
	-- will be backpropagated up the tree to the root using their 'Monoid'
	-- instance.
	--
	-- You may assume this function is only invoked in a state in which
	-- 'expand' would produce the empty list.
	, expand :: position -> m (Vector move)
	-- ^ Compute the moves available from this position. Return an empty vector
	-- to indicate that this is a leaf node (i.e. a won or lost position).
	, root :: m position
	-- ^ An action which can be used to regenerate a fresh, mutable copy of the
	-- game state you want to choose a move in.
	, turn :: position -> m player
	-- ^ Compute which player is currently making choices.
	, play :: position -> move -> m ()
	-- ^ Mutate the current position, making the given move.
	, select :: position -> Vector move -> m move
	-- ^ Randomly select one of the provided moves. Typically this will choose
	-- uniformly at random from the provided collection of moves, but a handle
	-- to the current position is provided in case you'd like to run a deep
	-- neural net to compute move priors or something crazy like that.
	--
	-- You may assume this function is only invoked with non-empty vectors
	-- returned from 'expand'.
	, preprocess :: position -> MCNode stats move summary -> m (stats, MCNode stats move summary)
	-- ^ Make adjustments to a game tree before doing a rollout for it. Under
	-- normal circumstances this should be 'emptyPreprocessor'.
	--
	-- While descending into a game tree to begin a rollout, you may make
	-- arbitrary adjustments to the tree. You should not modify the position
	-- you are given. Any statistics you produce here will be 'mappend'ed to
	-- the statistics produced by the rollout; the result will be used to
	-- update both the current node and backpropagated to its ancestors.
	--
	-- It is your responsibility to adjust the statistics of the children
	-- appropriately to maintain the invariant that @'nStatistics' = foldMap
	-- 'eStatistics' . 'nChildren'@. It is also your responsibility to make
	-- sure that any new 'Shared' children have keys in the cache.
	, summarize :: position -> m (Maybe summary)
	-- ^ Produce a pure summary of the current position. For positions
	-- summarized as 'Nothing', nothing special happens. If it is summarized
	-- with a 'Just', the position's statistics are stored in a separate
	-- 'HashMap' instead of inline in the tree; if there are multiple paths
	-- through the tree that reach the same summary, their statistics are
	-- combined. This lets the multiple paths share valuation information -- a
	-- rollout performed through any one path will inform the valuation
	-- computed in all other paths. See also 'neverSummarize'.
	--
	-- This is similar to a well-known technique invented by chess AI research
	-- called "transposition tables".
	--
	-- It is recommended that summaries be engineered to be very
	-- memory-compact, as typical runs of tree search will be storing really
	-- quite a lot of them in the 'HashMap'. Occasional collisions -- where
	-- strategically distinct positions have identical summaries -- can often
	-- be tolerated in the name of memory compactness. (Indeed, some research
	-- even finds that intentionally introducing collisions can improve
	-- performance, if the summary can be arranged so that the positions it
	-- lumps together are similar enough.)
	}

-- | Create an essentially empty tree, suitable for providing to 'mcts'.
emptyTree ::
	(Monad m, Monoid stats) =>
	MCTSParameters m stats score move position summary player ->
	m (MCTree stats move summary)
emptyTree params = do
	pos <- root params
	ms <- expand params pos
	pure MCTree
		{ tCache = Cache.empty
		, tRoot = MCNode
			{ nStatistics = mempty
			, nChildren = HM.empty
			, nUnexplored = V.toList ms
			}
		}

-- | Do no 'preprocess'ing at all.
emptyPreprocessor ::
	(Applicative m, Monoid stats) =>
	position -> tree -> m (stats, tree)
emptyPreprocessor _ t = pure (mempty, t)

-- | Never share information between different paths.
neverSummarize :: Applicative m => position -> m (Maybe summary)
neverSummarize _ = pure Nothing

-- | Perform one iteration of Monte Carlo tree search (called a rollout in the
-- literature). You should iterate this until you run out of computational
-- budget.
mcts ::
	(Monad m, Monoid stats, Hashable move, Eq move, Hashable summary, Eq summary, Ord score) =>
	MCTSParameters m stats score move position summary player ->
	MCTree stats move summary ->
	m (MCTree stats move summary)
mcts params MCTree { tCache = cache, tRoot = n } = do
	pos <- root params
	((_, n'), cache') <- runStateT (mcts_ params pos n) cache
	pure MCTree { tCache = cache', tRoot = n' }

maximumOn :: Ord a => (v -> a) -> HashMap k v -> Maybe (k, v)
-- checking for emptiness once at the beginning is cheaper than re-checking on
-- every iteration, as you would have to do if you folded with a Maybe
maximumOn f m = case HM.toList m of
	[] -> Nothing
	((k,v):_) -> Just . (\(k,v,a) -> (k,v)) $ HM.foldlWithKey'
		(\old@(k,v,a) k' v' -> let a' = f v' in if a' > a then (k',v',a') else old)
		(k,v,f v)
		m

mcts_ ::
	(Monad m, Monoid stats, Hashable move, Eq move, Hashable summary, Eq summary, Ord score) =>
	MCTSParameters m stats score move position summary player ->
	position ->
	MCNode stats move summary ->
	CacheT stats move summary m (stats, MCNode stats move summary)
mcts_ params pos = go where
	go n_ = do
		player <- lift (turn params pos)
		(stats, n) <- lift (preprocess params pos n_)
		case nUnexplored n of
			[] -> case maximumOn (\e -> score params player (nStatistics n) (eStatistics e)) (nChildren n) of
				Just (m, e) -> do
					lift (play params pos m)
					mn' <- gets (Cache.deref (eTarget e))
					n' <- case mn' of
						Just n' -> pure n'
						Nothing -> do -- should never happen
							ms <- lift (expand params pos)
							pure MCNode
								{ nStatistics = mempty
								, nChildren = HM.empty
								, nUnexplored = V.toList ms
								}
					(stats', n'') <- go n'
					tgt <- share (eTarget e) n''
					let stats'' = stats <> stats'
					    e' = e
					    	{ eStatistics = eStatistics e <> stats'
					    	, eTarget = tgt
					    	}
					    n' = n
					    	{ nStatistics = nStatistics n <> stats''
					    	, nChildren = HM.insert m e' (nChildren n)
					    	}
					pure (stats'', n')
				Nothing -> do
					stats' <- (stats<>) <$> lift (evaluate params pos)
					pure (stats', n)
			m:ms -> do
				lift (play params pos m)
				msummary <- lift (summarize params pos)
				ms' <- lift (expand params pos)
				stats' <- lift (rollout ms')
				let bothStats = stats <> stats'
				    tgt_ = case msummary of
				    	Nothing -> Unshared ()
				    	Just summary -> Shared summary
				tgt <- share tgt_ MCNode
					{ nStatistics = stats'
					, nChildren = HM.empty
					, nUnexplored = V.toList ms'
					}
				pure (bothStats, n
					{ nStatistics = nStatistics n <> bothStats
					, nChildren = HM.insert m (MCEdge stats' tgt) (nChildren n)
					, nUnexplored = ms
					})

	rollout ms | V.null ms = evaluate params pos
	rollout ms = do
		select params pos ms >>= play params pos
		expand params pos >>= rollout

-- | Compute the popular upper-confidence bound score.
--
-- Arguments are a visit count for the parent node (n in the literature), a
-- visit count for the current node (n_i in the literature), and a cumulative
-- utility achieved by children of the current node (Q_i in the literature).
-- The utility of individual leaves should be in the range [0, 1] (so that 0 <=
-- Q_i <= n_i).
ucb1 :: Double -> Double -> Double -> Double
ucb1 n n_i q_i = q_i/n_i + sqrt (2 * log n / n_i)

type CacheT stats move summary = StateT (Cache summary (MCNode stats move summary))

share :: (Hashable summary, Eq summary, Monad m) =>
	Shared summary node ->
	MCNode stats move summary ->
	CacheT stats move summary m (Shared summary (MCNode stats move summary))
share (Shared summary) n = Shared summary <$ modify (Cache.share summary n)
share (Unshared _) n = pure (Unshared n)
