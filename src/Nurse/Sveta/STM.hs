module Nurse.Sveta.STM (
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
import Data.Coerce

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
atomically = coerceSV Raw.atomically

retry :: STM a
retry = coerceS Raw.retry

orElse :: STM a -> STM a -> STM a
orElse = coerceSSS Raw.orElse

check :: Bool -> STM ()
check = coerceVS Raw.check

throwSTM :: Exception e => e -> STM a
throwSTM = coerceVS Raw.throwSTM

catchSTM :: Exception e => STM a -> (e -> STM a) -> STM a
catchSTM = coerceSOVSCS Raw.catchSTM

newTVar :: a -> STM (TVar a)
newTVar = coerceVS Raw.newTVar

readTVar :: TVar a -> STM a
readTVar = coerceVS Raw.readTVar

writeTVar :: TVar a -> a -> STM ()
writeTVar = coerceVVS Raw.writeTVar

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar = coerceVVS Raw.modifyTVar

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' = coerceVVS Raw.modifyTVar'

stateTVar :: TVar s -> (s -> (a, s)) -> STM a
stateTVar = coerceVVS Raw.stateTVar

swapTVar :: TVar a -> a -> STM a
swapTVar = coerceVVS Raw.swapTVar

newTMVar :: a -> STM (TMVar a)
newTMVar = coerceVS Raw.newTMVar

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = coerceS Raw.newEmptyTMVar

takeTMVar :: TMVar a -> STM a
takeTMVar = coerceVS Raw.takeTMVar

putTMVar :: TMVar a -> a -> STM ()
putTMVar = coerceVVS Raw.putTMVar

readTMVar :: TMVar a -> STM a
readTMVar = coerceVS Raw.readTMVar

writeTMVar :: TMVar a -> a -> STM ()
writeTMVar = coerceVVS Raw.writeTMVar

swapTMVar :: TMVar a -> a -> STM a
swapTMVar = coerceVVS Raw.swapTMVar

tryTakeTMVar :: TMVar a -> STM (Maybe a)
tryTakeTMVar = coerceVS Raw.tryTakeTMVar

tryPutTMVar :: TMVar a -> a -> STM Bool
tryPutTMVar = coerceVVS Raw.tryPutTMVar

tryReadTMVar :: TMVar a -> STM (Maybe a)
tryReadTMVar = coerceVS Raw.tryReadTMVar

isEmptyTMVar :: TMVar a -> STM Bool
isEmptyTMVar = coerceVS Raw.isEmptyTMVar

-- convention:
-- S for an (S)TM action
-- V for a non-STM (v)ariable
-- O for an open-parenthesis
-- C for a close-parenthesis

coerceS :: (Raw.STM a) -> (STM a)
coerceSV :: (Raw.STM a -> b) -> (STM a -> b)
coerceVS :: (a -> Raw.STM b) -> (a -> STM b)
coerceVVS :: (a -> b -> Raw.STM c) -> (a -> b -> STM c)
coerceSSS :: (Raw.STM a -> Raw.STM b -> Raw.STM c) -> (STM a -> STM b -> STM c)
coerceSOVSCS :: (Raw.STM a -> (b -> Raw.STM c) -> Raw.STM d) -> (STM a -> (b -> STM c) -> STM d)

coerceS = coerce
coerceSV = coerce
coerceVS = coerce
coerceVVS = coerce
coerceSSS = coerce
coerceSOVSCS = coerce
