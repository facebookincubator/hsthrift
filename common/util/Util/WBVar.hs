-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.WBVar (
  WBVar,
  newWBVar, newWBVarIO,
  readWBVar, writeWBVar, modifyWBVar', dirtyWBVar,
  writeBackWBVar,
  WBMVar,
  newEmptyWBMVar,
  newWBMVar,
  readWBMVar,
  takeWBMVar,
  writeWBMVar,
  withWBMVar,
  modifyWBMVar,
) where

import Util.Defer

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception

data WBVar a = WBVar
  { wbValue :: TVar a
  , wbDirty :: TVar Bool
  , wbWriting :: TVar Bool
  , wbWriteBack :: a -> IO ()
  }

newWBVar :: a -> (a -> IO ()) -> STM (WBVar a)
newWBVar x wb = WBVar
  <$> newTVar x
  <*> newTVar False
  <*> newTVar False
  <*> pure wb

newWBVarIO :: a -> (a -> IO ()) -> IO (WBVar a)
newWBVarIO x wb = WBVar
  <$> newTVarIO x
  <*> newTVarIO False
  <*> newTVarIO False
  <*> pure wb

readWBVar :: WBVar a -> STM a
readWBVar = readTVar . wbValue

writeWBVar :: WBVar a -> a -> Defer IO STM ()
writeWBVar v x = do
  lift $ writeTVar (wbValue v) x
  dirtyWBVar v

modifyWBVar' :: WBVar a -> (a -> a) -> Defer IO STM ()
modifyWBVar' v f = do
  lift $ modifyTVar' (wbValue v) f
  dirtyWBVar v

dirtyWBVar :: WBVar a -> Defer IO STM ()
dirtyWBVar v = do
  lift $ writeTVar (wbDirty v) True
  later $ writeBackWBVar v

-- Write back the variable if it is dirty.
--
-- NOTE: To avoid blocking an arbitrary number of threads, we'll keep writing
-- until the variable is no longer dirty. Suppose thread T1 is writing back
-- and T2 dirties the variable while this is happening and tries to write back
-- as well. In this situation, T2 will immediately return instead of blocking
-- until T1 is done. T1, on the other hand, will finish writing back, notice
-- that the variable is dirty and write back again.
writeBackWBVar :: WBVar a -> IO ()
writeBackWBVar v = do
  r <- atomically $ do
    dirty <- readTVar $ wbDirty v
    if dirty
      then do
        writing <- readTVar $ wbWriting v
        if writing
          then return Nothing
          else do
            writeTVar (wbDirty v) False
            writeTVar (wbWriting v) True
            Just <$> readTVar (wbValue v)
      else return Nothing
  case r of
    Just x -> do
      wbWriteBack v x
        `finally` atomically (writeTVar (wbWriting v) False)
        -- FIXME: A lame attempt to keep writing even in the case of an
        -- exception. I don't think this can really work without atomicity
        -- between the STM transaction and the write-back which seems impossible
        -- to achieve with STM.
        `onException` forkIO (writeBackWBVar v)
      writeBackWBVar v
    Nothing -> return ()


-- | A 'WBVar' that can be empty/full like 'MVar'
type WBMVar a = WBVar (Maybe a)

newWBMVar :: a -> (a -> IO ()) -> STM (WBMVar a)
newWBMVar a f = newWBVar (Just a) (maybe (return ()) f)

newEmptyWBMVar :: (a -> IO ()) -> STM (WBMVar a)
newEmptyWBMVar f = newWBVar Nothing (maybe (return ()) f)

readWBMVar :: WBMVar a -> STM a
readWBMVar v = readWBVar v >>= maybe retry return

takeWBMVar :: WBMVar a -> IO a
takeWBMVar v = atomically $ do
  a <- readWBMVar v
  writeTVar (wbValue v) Nothing
  return a

writeWBMVar :: WBMVar a -> a -> Defer IO STM ()
writeWBMVar v = writeWBVar v . Just

withWBMVar :: WBMVar a -> (a -> IO b) -> IO b
withWBMVar v =
  bracket (takeWBMVar v) (atomically . writeTVar (wbValue v) . Just)
  -- omit the write-back, we know the value didn't change.

modifyWBMVar :: WBMVar a -> (a -> IO (a,b)) -> IO b
modifyWBMVar v f =
  mask $ \restore -> do
    a <- takeWBMVar v
    (a',b) <- restore (f a)
      `onException` atomically (writeTVar (wbValue v) (Just a))
    immediately $ writeWBMVar v a'
    return b
