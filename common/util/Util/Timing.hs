{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Utilities for timing things and working with time differences
module Util.Timing
  ( reportTime
  , reportAndShowTime
  , showAllocs
  , showTime
  , timeIt
  , timeNF
  , timeItNoGC
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad.IO.Class
import Data.Int
import Text.Printf
import GHC.Stats
import GHC.Conc
import System.IO (stderr)

import Util.PrettyPrint
import Util.Time

-- | Runs an 'IO' operation and prints how long it took. Also returns
-- the timing value for use.
reportAndShowTime :: MonadIO m => String -> m a -> m (Double, Int64, a)
reportAndShowTime name io = do
  (t, b, a) <- timeIt io
  liftIO $ hPrintf stderr "%s: %s, %s\n" name (showTime t) (showAllocs b)
  return (t, b, a)

-- | Runs an 'IO' operation and reports how long it took. Useful for
-- ad-hoc benchmarking.
reportTime :: MonadIO m => String -> m a -> m a
reportTime name io = do
  (_, _, a) <- reportAndShowTime name io
  return a

-- | Converts time in seconds to a human friendly string.
showTime :: Double -> String
showTime t = printf "%.2f%s" val unit
  where
   unit :: String
   (val, unit)
        | t >= 1    = (t, "s")
        | t >= 1e-3 = (t * 1e3, "ms")
        | t >= 1e-6 = (t * 1e6, "us")
        | otherwise = (t * 1e9, "ns")

-- | Converts a number of bytes to a human-friendsly string.
showAllocs :: Int64 -> String
showAllocs = renderBytesString . fromIntegral

-- | Runs an 'IO' action and returns a triple of the time it consumed (in sec),
-- the number of bytes it allocated, and the result it returned.
timeIt :: MonadIO m => m a -> m (Double, Int64, a)
timeIt action = do
  t0 <- liftIO getTimePoint
  a0 <- liftIO getAllocationCounter
  ret <- action
  a1 <- liftIO getAllocationCounter
  t <- liftIO $ getElapsedTime t0
  return (toDiffSeconds t, a0 - a1, ret)

-- | Runs an 'IO' action, reduces the result to normal form and returns a pair
-- of the time it consumed and the result it returned.
timeNF :: NFData a => IO a -> IO (Double, Int64, a)
timeNF action = timeIt $ evaluate . force =<< action

-- | Records the time to run an action excluding GC time.  Probably
-- more expensive than 'timeIt'.
timeItNoGC :: IO a -> IO (Double, Int64, a)
timeItNoGC io = do
  t0 <- getRTSStats
  a0 <- getAllocationCounter
  ret <- io
  a1 <- getAllocationCounter
  t1 <- getRTSStats
  let diff_ns = mutator_elapsed_ns t1 - mutator_elapsed_ns t0
  return (fromIntegral diff_ns / 1000000000, a0 - a1, ret)
