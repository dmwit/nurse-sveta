module Dr.Mario.Sveta.MCTS (
	MCTree(..),
	MCTSParameters(..),
	emptyTree,
	mcts,
	ucb1,
	-- * Convenience re-exports
	Down(..),
	NonEmpty(..)
	) where

import Data.Hashable
import Data.HashPSQ (HashPSQ)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Ord
import Data.Vector (Vector)
import qualified Data.HashPSQ as Q
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

-- | A representation of the current state of a run of Monte Carlo tree search.
data MCTree stats score move = MCTree
	{ statistics :: stats
	, children :: HashPSQ move score (MCTree stats score move)
	, unexplored :: [move]
	} deriving (Eq, Show)

data MCTSParameters m stats score move position player = MCTSParameters
	{ score :: player -> stats -> stats -> score
	-- ^ Given which player is currently choosing a move and the statistics for
	-- the parent node and current node, compute a priority score for searching
	-- this node in the future. Smaller scores are searched first.
	--
	-- See also 'ucb1'.
	, evaluate :: position -> m stats
	-- ^ Given a leaf position, compute some statistics. These statistics
	-- typically include the utilities for each player and a count of times
	-- visited (which this function should initialize to 1). The statistics
	-- will be backpropogated up the tree to the root using their 'Semigroup'
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
	}

-- | Create an essentially empty tree, suitable for providing to 'mcts'.
emptyTree ::
	(Monad m, Monoid stats) =>
	MCTSParameters m stats score move position player ->
	m (MCTree stats score move)
emptyTree params = do
	pos <- root params
	ms <- expand params pos
	pure MCTree
		{ statistics = mempty
		, children = Q.empty
		, unexplored = V.toList ms
		}

-- | Perform one iteration of Monte Carlo tree search (called a rollout in the
-- literature). You should iterate this until you run out of computational
-- budget.
mcts ::
	(Monad m, Semigroup stats, Hashable move, Ord move, Ord score) =>
	MCTSParameters m stats score move position player ->
	MCTree stats score move ->
	m (MCTree stats score move)
mcts params t = do
	pos <- root params
	(_, t') <- mcts_ params pos t
	pure t'

mcts_ ::
	(Monad m, Semigroup stats, Hashable move, Ord move, Ord score) =>
	MCTSParameters m stats score move position player ->
	position ->
	MCTree stats score move ->
	m (stats, MCTree stats score move)
mcts_ params pos = go where
	go t = case unexplored t of
		[] -> case Q.minView (children t) of
			Just (m, _, t', q) -> do
				player <- turn params pos
				play params pos m
				(stats, t'') <- go t'
				let stats' = statistics t <> stats
				pure $ (stats, t
					{ statistics = stats'
					, children = Q.insert m (score params player stats' (statistics t'')) t'' q
					})
			Nothing -> do
				stats <- evaluate params pos
				pure $ (stats, t { statistics = statistics t <> stats })
		m:ms -> do
			player <- turn params pos
			play params pos m
			ms' <- expand params pos
			stats <- rollout ms'
			let stats' = statistics t <> stats
			pure (stats, t
				{ statistics = stats'
				, children = Q.insert m (score params player stats' stats) (MCTree stats Q.empty (V.toList ms')) (children t)
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
ucb1 :: Double -> Double -> Double -> Down Double
ucb1 n n_i q_i = Down $ q_i/n_i + sqrt (2 * log n / n_i)
