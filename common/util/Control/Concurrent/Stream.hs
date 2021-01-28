-- Copyright (c) Facebook, Inc. and its affiliates.

-- | Higher level concurrency facilities for multiple workers concurrently
-- over a streaming source of input

module Control.Concurrent.Stream
  ( stream
  , streamBound
  , streamWithState
  , streamWithStateBound
  , streamWithThrow
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import Util.Control.Exception
import Util.Log

data ShouldBindThreads = BoundThreads | UnboundThreads

data ShouldThrow = ThrowExceptions | SwallowExceptions

-- | Maps workers concurrently over a stream of values with a bounded size
--
-- Runs the producer until it terminates, passing in a function to add things
-- into the stream. Runs at most `maxConcurrency` threads simultaneously to
-- process things put into the stream.
-- There's no end aggregation for the output from each worker, which doesn't
-- make this composable. We can add that in the future when needed.
--
-- Note that these functions swallow exceptions in the workers, except for
-- `streamWithThrow`.
--
-- `conduit` and `pipes` provide functionality for running consecutive stages
-- in parallel, but nothing for running a single stage concurrently.
stream
  :: Int -- ^ Maximum Concurrency
  -> ((a -> IO ()) -> IO ()) -- ^ Producer
  -> (a -> IO ()) -- ^ Worker
  -> IO ()
stream maxConcurrency producer worker = stream_ UnboundThreads
  SwallowExceptions producer (replicate maxConcurrency ()) $ const worker

-- | Like stream, but uses bound threads for the workers.  See
-- 'Control.Concurrent.forkOS' for details on bound threads.
streamBound
  :: Int -- ^ Maximum Concurrency
  -> ((a -> IO ()) -> IO ()) -- ^ Producer
  -> (a -> IO ()) -- ^ Worker
  -> IO ()
streamBound maxConcurrency producer worker = stream_ BoundThreads
  SwallowExceptions producer (replicate maxConcurrency ()) $ const worker

-- | Like stream, but each worker keeps a state: the state can be a parameter
-- to the worker function, or a state that you can build upon (for example the
-- state can be an IORef of some sort)
-- There will be a thread per worker state
streamWithState
  :: ((a -> IO ()) -> IO ()) -- ^ Producer
  -> [b] -- ^ Worker state
  -> (b -> a -> IO ()) -- ^ Worker
  -> IO ()
streamWithState = stream_ UnboundThreads SwallowExceptions

-- | Like streamWithState but uses bound threads for the workers.
streamWithStateBound
  :: ((a -> IO ()) -> IO ()) -- ^ Producer
  -> [b] -- ^ Worker state
  -> (b -> a -> IO ()) -- ^ Worker
  -> IO ()
streamWithStateBound = stream_ BoundThreads SwallowExceptions

-- | Like `stream`, but if a worker throws a synchronous exception, it will be
-- propagated to the caller.
streamWithThrow
  :: Int -- ^ Maximum Concurrency
  -> ((a -> IO ()) -> IO ()) -- ^ Producer
  -> (a -> IO ()) -- ^ Worker
  -> IO ()
streamWithThrow maxConcurrency producer worker = stream_ UnboundThreads
  ThrowExceptions producer (replicate maxConcurrency ()) $ const worker

stream_
  :: ShouldBindThreads -- use bound threads?
  -> ShouldThrow -- propagate worker exceptions?
  -> ((a -> IO ()) -> IO ()) -- ^ Producer
  -> [b] -- Worker state
  -> (b -> a -> IO ()) -- ^ Worker
  -> IO ()
stream_ useBoundThreads shouldThrow producer workerStates worker = do
  let maxConcurrency = length workerStates
  q <- atomically $ newTBQueue (fromIntegral maxConcurrency)
  let write x = atomically $ writeTBQueue q (Just x)
  mask $ \unmask ->
    concurrently_ (runWorkers unmask q) $ unmask $ do
      -- run the producer
      producer write
      -- write end-markers for all workers
      replicateM_ maxConcurrency $
        atomically $ writeTBQueue q Nothing
  where
    runWorkers unmask q = case useBoundThreads of
      BoundThreads ->
         foldr1 concurrentlyBound $ map (runWorker unmask q) workerStates
      UnboundThreads -> mapConcurrently_ (runWorker unmask q) workerStates

    concurrentlyBound l r =
      withAsyncBound l $ \a ->
      withAsyncBound r $ \b ->
      void $ waitBoth a b

    runWorker unmask q s = do
      v <- atomically $ readTBQueue q
      case v of
        Nothing -> return ()
        Just t -> do
          e <- tryAll $ unmask $ worker s t
          case e of
            Left ex -> case shouldThrow of
              ThrowExceptions -> throw ex
              SwallowExceptions -> logError $ show ex
            Right _ -> return ()
          runWorker unmask q s
