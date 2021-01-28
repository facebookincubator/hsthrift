-- Copyright (c) Facebook, Inc. and its affiliates.

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Util.Concurrent
  ( concurrently3
  , concurrently4
  , concurrently5
  , concurrently6
  , concurrently7
  -- * ThreadLock
  , ThreadLock
  , newThreadLock
  , withThreadLock
  -- * Caching
  , cacheSuccess
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (void)
import Control.Monad.IO.Class

import Exception (ExceptionMonad(..))

c :: IO a -> IO b -> IO (a, b)
c = concurrently

concurrently3 :: IO t1 -> IO t2 -> IO t3 -> IO (t1, t2, t3)
concurrently3 a1 a2 a3 = flatten3 <$> a1 `c` a2 `c` a3

concurrently4 :: IO t1 -> IO t2 -> IO t3 -> IO t4 -> IO (t1, t2, t3, t4)
concurrently4 a1 a2 a3 a4 = flatten4 <$> a1 `c` a2 `c` a3 `c` a4

concurrently5 :: IO t1 -> IO t2 -> IO t3 -> IO t4 -> IO t5
              -> IO (t1, t2, t3, t4, t5)
concurrently5 a1 a2 a3 a4 a5 = flatten5 <$> a1 `c` a2 `c` a3 `c` a4 `c` a5

concurrently6 :: IO t1 -> IO t2 -> IO t3 -> IO t4 -> IO t5 -> IO t6
              -> IO (t1, t2, t3, t4, t5, t6)
concurrently6 a1 a2 a3 a4 a5 a6 =
  flatten6 <$> a1 `c` a2 `c` a3 `c` a4 `c` a5 `c` a6

concurrently7 :: IO t1 -> IO t2 -> IO t3 -> IO t4 -> IO t5 -> IO t6 -> IO t7
              -> IO (t1, t2, t3, t4, t5, t6, t7)
concurrently7 a1 a2 a3 a4 a5 a6 a7 =
  flatten7 <$> a1 `c` a2 `c` a3 `c` a4 `c` a5 `c` a6 `c` a7

flatten3 ((a1, a2), a3) = (a1, a2, a3)
flatten4 (((a1, a2), a3), a4) = (a1, a2, a3, a4)
flatten5 ((((a1, a2), a3), a4), a5) = (a1, a2, a3, a4, a5)
flatten6 (((((a1, a2), a3), a4), a5), a6) = (a1, a2, a3, a4, a5, a6)
flatten7 ((((((a1, a2), a3), a4), a5), a6), a7) = (a1, a2, a3, a4, a5, a6, a7)

newtype ThreadLock = ThreadLock (IO (IO ()))

newThreadLock :: IO ThreadLock
newThreadLock = do
  mvar <- newEmptyMVar
  return $ ThreadLock $ do
    ours <- myThreadId
    theirs <- tryReadMVar mvar
    if theirs == Just ours
    then return $ return () -- we are holding the lock, so this is noop
    else do
      putMVar mvar ours
      return $ void $ takeMVar mvar

-- | withThreadLock guarantees that only one thread holds the lock at the same
-- time. It can be nested so it is safe to use it in a recursive function.
withThreadLock :: ExceptionMonad m => ThreadLock -> m a -> m a
withThreadLock (ThreadLock takeIO) action = do
  gbracket (liftIO takeIO) (\releaseIO -> liftIO releaseIO) $ const action

-- | Build an IO action that will cache its result the first time it
-- runs successfullly to completion. If it fails with an exception,
-- the exception is re-raised, but the IO action will be attempted
-- again the next time it is invoked.
--
-- Like `Control.Concurrent.Extra.once`, but without caching exceptions.
--
cacheSuccess :: IO a -> IO (IO a)
cacheSuccess act = do
  m <- newMVar Nothing
  return $ modifyMVar m $ \x -> case x of
    Nothing -> do a <- act; return (Just a, a)
    Just a -> return (Just a, a)
