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
serviceCalls :: Procedure a b -> (forall t. Traversable t => t a -> IO (t b, c)) -> IO c
serviceCalls proc f = join (atomically (serviceCallsSTM proc f))

-- | Pop all the pending calls and return an IO action that will service them
-- all and reply. Usually you want 'serviceCalls' instead, but since this
-- blocks waiting for at least one request, occasionally you want to be able to
-- use 'orElse' to short circuit this.
serviceCallsSTM :: Procedure a b -> (forall t. Traversable t => t a -> IO (t b, c)) -> STM (IO c)
serviceCallsSTM (Procedure bf) f = process <$> popAll bf where
	process reqs = do
		(bs, c) <- f (map fst reqs)
		c <$ zipWithM_ ((atomically .) . writeTMVar) (map snd reqs) bs

-- | Send a request for processing. This will block until the request is
-- finished processing, which includes waiting for the request to be accepted
-- and waiting for the actual computation to be done on the request.
call :: Procedure a b -> a -> IO b
call proc a = join (schedule proc a)

-- | Send a request for processing. This immediately returns an 'IO' action
-- that will block until the request is finished processing.
schedule :: Procedure a b -> a -> IO (IO b)
schedule (Procedure bf) a = do
	v <- newEmptyTMVarIO
	evaluate a
	atomically (push (a, v) bf)
	pure (atomically (takeTMVar v))
