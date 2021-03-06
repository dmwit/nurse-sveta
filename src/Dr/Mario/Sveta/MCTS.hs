module Dr.Mario.Sveta.MCTS (
	MCTree(..),
	MCTSParameters(..),
	emptyTree,
	mcts,
	ucb1,
	emptyPreprocessor,
	) where

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

-- | A representation of the current state of a run of Monte Carlo tree search.
data MCTree stats move = MCTree
	{ statistics :: stats
	, children :: HashMap move (MCTree stats move)
	, unexplored :: [move]
	} deriving (Eq, Show)

data MCTSParameters m stats score move position = MCTSParameters
	{ score :: move -> stats -> stats -> score
	-- ^ Given a move and the statistics for the parent node and current node,
	-- compute a priority score for searching this node in the future. Larger
	-- scores are searched first.
	--
	-- See also 'ucb1'.
	, evaluate :: position -> m stats
	-- ^ Given a leaf position, compute some statistics. These statistics
	-- typically include the utilities for each player and a count of times
	-- visited (which this function should initialize to 1). The statistics
	-- will be backpropagated up the tree to the root using their 'Semigroup'
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
	, preprocess :: position -> MCTree stats move -> m (stats, MCTree stats move)
	-- ^ Make adjustments to a game tree before doing a rollout for it. Under
	-- normal circumstances this should be 'emptyPreprocessor'.
	--
	-- While descending into a game tree to begin a rollout, you may make
	-- arbitrary adjustments to the tree. You should not modify the position
	-- you are given. Any statistics you produce here will be 'mappend'ed to
	-- the statistics produced by the rollout; the result will be used to
	-- update both the current node and backpropagated to its ancestors.
	}

-- | Create an essentially empty tree, suitable for providing to 'mcts'.
emptyTree ::
	(Monad m, Monoid stats) =>
	MCTSParameters m stats score move position ->
	m (MCTree stats move)
emptyTree params = do
	pos <- root params
	ms <- expand params pos
	pure MCTree
		{ statistics = mempty
		, children = HM.empty
		, unexplored = V.toList ms
		}

-- | Do no 'preprocess'ing at all.
emptyPreprocessor ::
	(Applicative m, Monoid stats) =>
	position -> tree -> m (stats, tree)
emptyPreprocessor _ t = pure (mempty, t)

-- | Perform one iteration of Monte Carlo tree search (called a rollout in the
-- literature). You should iterate this until you run out of computational
-- budget.
mcts ::
	(Monad m, Semigroup stats, Hashable move, Eq move, Ord score) =>
	MCTSParameters m stats score move position ->
	MCTree stats move ->
	m (MCTree stats move)
mcts params t = do
	pos <- root params
	(_, t') <- mcts_ params pos t
	pure t'

maximumOn :: Ord a => (k -> v -> a) -> HashMap k v -> Maybe (k, v)
-- checking for emptiness once at the beginning is cheaper than re-checking on
-- every iteration, as you would have to do if you folded with a Maybe
maximumOn f m = case HM.toList m of
	[] -> Nothing
	((k,v):_) -> Just . (\(k,v,a) -> (k,v)) $ HM.foldlWithKey'
		(\old@(k,v,a) k' v' -> let a' = f k' v' in if a' > a then (k',v',a') else old)
		(k,v,f k v)
		m

mcts_ ::
	(Monad m, Semigroup stats, Hashable move, Eq move, Ord score) =>
	MCTSParameters m stats score move position ->
	position ->
	MCTree stats move ->
	m (stats, MCTree stats move)
mcts_ params pos = go where
	go t_ = do
		(stats, t) <- preprocess params pos t_
		case unexplored t of
			[] -> case maximumOn (\m t' -> score params m (statistics t) (statistics t')) (children t) of
				Just (m, t') -> do
					play params pos m
					(stats', t'') <- go t'
					let stats'' = stats <> stats'
					pure (stats'', t
						{ statistics = statistics t <> stats''
						, children = HM.insert m t'' (children t)
						})
				Nothing -> do
					stats' <- (stats<>) <$> evaluate params pos
					pure (stats', t { statistics = statistics t <> stats' })
			m:ms -> do
				play params pos m
				ms' <- expand params pos
				stats' <- rollout ms'
				let bothStats = stats <> stats'
				pure (bothStats, t
					{ statistics = statistics t <> bothStats
					, children = HM.insert m (MCTree stats' HM.empty (V.toList ms')) (children t)
					, unexplored = ms
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
