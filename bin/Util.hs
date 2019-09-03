module Util where

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Applicative
import Dr.Mario.Sveta
import Dr.Mario.Sveta.MCTS
import System.Random.MWC
import qualified Dr.Mario.Model as M

randomBoard :: GenIO -> IO M.Board
randomBoard gen = liftA2 M.randomBoard
	(uniformR (2, maxBound) gen) (uniformR (0, 20) gen)

randomPill :: GenIO -> IO (M.Color, M.Color)
randomPill gen = liftA2 (,)
	(M.decodeColor <$> uniformR (2, maxBound) gen)
	(M.decodeColor <$> uniformR (2, maxBound) gen)

data Repetitions = Finite Int | Infinity deriving (Eq, Ord, Read, Show)

toggleFiniteness :: Repetitions -> Repetitions
toggleFiniteness (Finite _) = Infinity
toggleFiniteness Infinity = Finite 0

increment :: Repetitions -> Repetitions
increment (Finite n) = Finite (n+1)
increment Infinity = Infinity

decrement :: Repetitions -> Repetitions
decrement (Finite n) = Finite (n-1)
decrement Infinity = Infinity

data Comms = Comms
	{ tree :: DrMarioTree
	, params :: DrMarioParameters
	, repetitions :: Repetitions
	, sequenceNumber :: Int
	, iterations :: Double
	}

mctsThread :: TVar Comms -> IO ()
mctsThread commsRef = forever $ do
	c <- atomically $ do
		c <- readTVar commsRef
		guard (repetitions c > Finite 0)
		pure c
	t <- mcts (params c) (tree c)
	atomically $ do
		c' <- readTVar commsRef
		if sequenceNumber c == sequenceNumber c'
		then writeTVar commsRef c
			{ tree = t
			, repetitions = decrement (repetitions c)
			, iterations = iterations c + 1
			}
		else pure ()
