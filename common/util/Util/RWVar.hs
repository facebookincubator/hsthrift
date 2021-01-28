-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.RWVar
  ( RWVar
  , newRWVar
  , withReadRWVar
  , withWriteRWVar
  ) where

import qualified Control.Concurrent.ReadWriteLock as L
import Data.IORef

-- | Var holding a value that's only accessible given access to the lock
-- The lock implementation can starve writers
data RWVar a = RWVar (IORef a) L.RWLock

-- -----------------------------------------------------------------------------
-- Creation

-- | Creates a new var that can be read from or written to
newRWVar :: a -> IO (RWVar a)
newRWVar value = do
  lock <- L.new
  ref <- newIORef value
  return $ RWVar ref lock

-- -----------------------------------------------------------------------------
-- Reading

-- | Obtains a read lock and runs the action with the held value
withReadRWVar :: RWVar a -> (a -> IO b) -> IO b
withReadRWVar (RWVar value lock) action =
  L.withRead lock $ action =<< readIORef value

-- -----------------------------------------------------------------------------
-- Writing

-- | Obtains a write lock and runs the action with the held value.
-- Overwrites the internal value with the newly computed value.
withWriteRWVar :: RWVar a -> (a -> IO (a, b)) -> IO b
withWriteRWVar (RWVar value lock) action =
  L.withWrite lock $ do
    old <- readIORef value
    (new, ret) <- action old
    writeIORef value new
    return ret
