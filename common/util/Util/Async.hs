-- Copyright (c) Facebook, Inc. and its affiliates.

-- | This module provides some higher level concurrency facilities
module Util.Async
  ( window_,
    windowUnorderedReduce,
    runHereOrConcurrently
  ) where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad (forM_, replicateM_)

-- | This function is similar to folly::window. It takes a list of input items
--   and launch a pool of `maxConcurrency` of workers to process them
window_ :: Int -> (a -> IO ()) -> [a] -> IO ()
window_ maxConcurrency process tasks = do
  queue <- newChan
  forM_ tasks $ writeChan queue . Just
  replicateM_ maxConcurrency $ writeChan queue Nothing
  replicateConcurrently_ maxConcurrency $ runWorker queue process
  where
    runWorker :: Chan (Maybe a) -> (a -> IO ()) -> IO ()
    runWorker queue process' = do
      token <- readChan queue
      case token of
        Just task -> do
          process' task
          runWorker queue process'
        Nothing -> return ()


-- | A variant of window function where we collect the result without
--   preserving the order
windowUnorderedReduce
  :: Monoid b => Int -> (a -> IO b) -> [a] -> IO [b]
windowUnorderedReduce maxConcurrency process tasks = do
  queue <- newChan
  forM_ tasks $ writeChan queue . Just
  replicateM_ maxConcurrency $ writeChan queue Nothing
  replicateConcurrently maxConcurrency
    $ runWorker queue mempty
  where
    runWorker queue acc = do
      token <- readChan queue
      case token of
        Just task -> do
          result <- process task
          runWorker queue $ mappend result acc
        Nothing -> return acc

-- | A wrapper for 'mapConcurrently' so that a singleton list of
-- actions runs in this thread, instead of asynchronously
runHereOrConcurrently :: [IO a] -> IO [a]
runHereOrConcurrently [] = return []
runHereOrConcurrently [x] = (:[]) <$> x
runHereOrConcurrently xs = mapConcurrently id xs
