module Dr.Mario.STM (
	-- * STM
	STM(..),
	atomically,
	retry, orElse, check,
	throwSTM, catchSTM,
	-- * TVar
	TVar,
	newTVar, Raw.newTVarIO,
	readTVar, Raw.readTVarIO, writeTVar,
	modifyTVar, modifyTVar', stateTVar, swapTVar,
	Raw.registerDelay, Raw.mkWeakTVar,
	-- * TMVar
	TMVar,
	newTMVar, newEmptyTMVar, Raw.newTMVarIO, Raw.newEmptyTMVarIO,
	takeTMVar, putTMVar, readTMVar, writeTMVar,
	swapTMVar,
	tryTakeTMVar, tryPutTMVar, tryReadTMVar,
	isEmptyTMVar,
	Raw.mkWeakTMVar,
	) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Data.Array.MArray

import Control.Concurrent.STM.TArray (TArray)
import Control.Concurrent.STM.TMVar (TMVar)
import Control.Concurrent.STM.TVar (TVar)

import qualified Control.Concurrent.STM as Raw
import qualified Control.Concurrent.STM.TArray as Raw
import qualified Control.Concurrent.STM.TMVar as Raw
import qualified Control.Concurrent.STM.TVar as Raw

-- | Just like 'Raw.STM', but pattern match failures call 'retry'.
newtype STM a = Infallible { fallible :: Raw.STM a } deriving
	( Functor, Applicative, Monad
	, Alternative, MonadPlus
	, MonadFix, MArray TArray e
	, Semigroup, Monoid
	)

instance MonadFail STM where fail _ = Infallible Raw.retry

atomically :: STM a -> IO a
atomically = Raw.atomically . fallible

retry :: STM a
retry = Infallible Raw.retry

orElse :: STM a -> STM a -> STM a
orElse (Infallible a) (Infallible a') = Infallible (Raw.orElse a a')

check :: Bool -> STM ()
check = Infallible . Raw.check

throwSTM :: Exception e => e -> STM a
throwSTM = Infallible . Raw.throwSTM

catchSTM :: Exception e => STM a -> (e -> STM a) -> STM a
catchSTM (Infallible a) f = Infallible $ Raw.catchSTM a (fallible . f)

newTVar :: a -> STM (TVar a)
newTVar = Infallible . Raw.newTVar

readTVar :: TVar a -> STM a
readTVar = Infallible . Raw.readTVar

writeTVar :: TVar a -> a -> STM ()
writeTVar t = Infallible . Raw.writeTVar t

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar t = Infallible . Raw.modifyTVar t

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' t = Infallible . Raw.modifyTVar' t

stateTVar :: TVar s -> (s -> (a, s)) -> STM a
stateTVar t = Infallible . Raw.stateTVar t

swapTVar :: TVar a -> a -> STM a
swapTVar t = Infallible . Raw.swapTVar t

newTMVar :: a -> STM (TMVar a)
newTMVar = Infallible . Raw.newTMVar

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = Infallible Raw.newEmptyTMVar

takeTMVar :: TMVar a -> STM a
takeTMVar = Infallible . Raw.takeTMVar

putTMVar :: TMVar a -> a -> STM ()
putTMVar t = Infallible . Raw.putTMVar t

readTMVar :: TMVar a -> STM a
readTMVar = Infallible . Raw.readTMVar

writeTMVar :: TMVar a -> a -> STM ()
writeTMVar t = Infallible . Raw.writeTMVar t

swapTMVar :: TMVar a -> a -> STM a
swapTMVar t = Infallible . Raw.swapTMVar t

tryTakeTMVar :: TMVar a -> STM (Maybe a)
tryTakeTMVar = Infallible . Raw.tryTakeTMVar

tryPutTMVar :: TMVar a -> a -> STM Bool
tryPutTMVar t = Infallible . Raw.tryPutTMVar t

tryReadTMVar :: TMVar a -> STM (Maybe a)
tryReadTMVar = Infallible . Raw.tryReadTMVar

isEmptyTMVar :: TMVar a -> STM Bool
isEmptyTMVar = Infallible . Raw.isEmptyTMVar
