module Dr.Mario.STM.BatchProcessor where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Dr.Mario.STM

data BoundedFIFO a = BoundedFIFO
	{ bound :: Int
	, fifo :: TVar (Int, [a])
	}

emptyBoundedFIFO :: Int -> IO (BoundedFIFO a)
emptyBoundedFIFO n = BoundedFIFO n <$> newTVarIO (0, [])

-- | Blocks when full.
push :: a -> BoundedFIFO a -> STM ()
push a bf = do
	(n, as) <- readTVar (fifo bf)
	check (n < bound bf)
	writeTVar (fifo bf) (n+1, a:as)

-- | Blocks until at least one thing is available.
popAll :: BoundedFIFO a -> STM [a]
popAll bf = do
	(_, as@(_:_)) <- readTVar (fifo bf)
	writeTVar (fifo bf) (0, [])
	pure as

newtype Procedure a b = Procedure { comms :: BoundedFIFO (a, TMVar b) }

-- | Create a new handle for batch processing. The first argument says how many
-- outstanding requests are allowed. You should fork a consumer shortly after
-- that calls `serviceCalls` in a loop.
newProcedure :: Int -> IO (Procedure a b)
newProcedure n = Procedure <$> emptyBoundedFIFO n

-- | Pop all the pending calls, and service them all at once, then return. You
-- might want to combine with, for example, 'forkIO' and 'forever'.
serviceCalls :: Procedure a b -> (forall t. Traversable t => t a -> IO (t b)) -> IO ()
serviceCalls (Procedure bf) f = do
	(args, rets) <- unzip <$> atomically (popAll bf)
	bs <- f args
	zipWithM_ ((atomically .) . writeTMVar) rets bs

-- | Send a request for processing. This will block until the request is
-- finished processing, which includes waiting for the request to be accepted
-- and waiting for the actual computation to be done on the request.
call :: Procedure a b -> a -> IO b
call (Procedure bf) a = do
	v <- newEmptyTMVarIO
	evaluate a
	atomically (push (a, v) bf)
	atomically (takeTMVar v)
