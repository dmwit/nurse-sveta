module Nurse.Sveta.STM.BatchProcessor where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Functor
import Data.Vector (Vector)
import Nurse.Sveta.STM

import qualified Data.Vector as V

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
	pure (reverse as)

newtype Procedure a b = Procedure { comms :: BoundedFIFO (a, TMVar b) }

-- | Create a new handle for batch processing. The first argument says how many
-- outstanding requests are allowed. You should fork a consumer shortly after
-- that calls `serviceCalls` in a loop.
newProcedure :: Int -> IO (Procedure a b)
newProcedure n = Procedure <$> emptyBoundedFIFO n

-- Once upon a time, all these Vectors were forall t. Traversable t's, to
-- ensure that nothing hinky could happen with the servicer producing fewer (or
-- more) results than requests. Sadly, ToEndpoint really needs its contents to
-- be in Vectors because it's going to be doing indexing. The real world
-- strikes again.

-- | Pop all the pending calls, and service them all at once, then return. You
-- might want to combine with, for example, 'forkIO' and 'forever'.
serviceCalls :: Procedure a b -> (Vector a -> IO (Vector b, c)) -> IO c
serviceCalls proc f = join (atomically (serviceCallsSTM proc f))

-- | A convenience wrapper around 'serviceCalls' that doesn't compute anything
-- about the inputs.
serviceCalls_ :: Procedure a b -> (Vector a -> IO (Vector b)) -> IO ()
serviceCalls_ proc f = serviceCalls proc (\as -> f as <&> \bs -> (bs, ()))

-- | Pop all the pending calls and return an IO action that will service them
-- all and reply. Usually you want 'serviceCalls' instead, but since this
-- blocks waiting for at least one request, occasionally you want to be able to
-- use 'orElse' to short circuit this.
serviceCallsSTM :: Procedure a b -> (Vector a -> IO (Vector b, c)) -> STM (IO c)
serviceCallsSTM (Procedure bf) f = process <$> popAll bf where
	process reqs = do
		(bs, c) <- f as
		c <$ V.zipWithM_ ((atomically .) . writeTMVar) resps bs
		where
		(as, resps) = V.unzip (V.fromList reqs)

-- | A convenience wrapper around 'serviceCallsSTM' that doesn't compute
-- anything about the inputs.
serviceCallsSTM_ :: Procedure a b -> (Vector a -> IO (Vector b)) -> STM (IO ())
serviceCallsSTM_ proc f = serviceCallsSTM proc (\as -> f as <&> \bs -> (bs, ()))

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
