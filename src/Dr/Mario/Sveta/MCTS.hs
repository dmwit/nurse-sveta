module Dr.Mario.Sveta.MCTS where

import Data.Hashable
import Data.HashPSQ (HashPSQ)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.HashPSQ as Q
import qualified Data.List.NonEmpty as NE

data MCTree stats score move = MCTree
	{ statistics :: stats
	, children :: HashPSQ move score (MCTree stats score move)
	, unexplored :: [move]
	} deriving (Eq, Show)

data MCTSParameters m stats score move position player = MCTSParameters
	{ score :: player -> stats -> score
	, evaluate :: position -> m stats
	, expand :: position -> m [move]
	, root :: m position
	, turn :: position -> m player
	, play :: position -> move -> m ()
	, select :: position -> NonEmpty move -> m move
	}

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
		, unexplored = ms
		}

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
				pure $ (stats, t
					{ statistics = statistics t <> stats
					, children = Q.insert m (score params player (statistics t'')) t'' q
					})
			Nothing -> do
				stats <- evaluate params pos
				pure $ (stats, t { statistics = statistics t <> stats })
		m:ms -> do
			player <- turn params pos
			play params pos m
			ms' <- expand params pos
			stats <- rollout ms'
			pure (stats, t
				{ statistics = statistics t <> stats
				, children = Q.insert m (score params player stats) (MCTree stats Q.empty ms') (children t)
				, unexplored = ms
				})

	rollout [] = evaluate params pos
	rollout (m:ms) = do
		select params pos (m:|ms) >>= play params pos
		expand params pos >>= rollout
