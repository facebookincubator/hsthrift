-- Copyright (c) Facebook, Inc. and its affiliates.

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
import Data.Time.Clock
import Data.Int
import Text.Printf
import GHC.Stats
import GHC.Conc

-- | Runs an 'IO' operation and prints how long it took. Also returns
-- the timing value for use.
reportAndShowTime :: String -> IO a -> IO (Double, Int64, a)
reportAndShowTime name io = do
  (t, b, a) <- timeIt io
  printf "%s: %s, %s\n" name (showTime t) (showAllocs b)
  return (t, b, a)

-- | Runs an 'IO' operation and reports how long it took. Useful for
-- ad-hoc benchmarking.
reportTime :: String -> IO a -> IO a
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
showAllocs b
 | b' > gb = printf "%.2f GB" (b' / gb)
 | b' > mb = printf "%.2f MB" (b' / mb)
 | b' > kb = printf "%.2f kB" (b' / kb)
 | otherwise = printf "%d bytes" (fromIntegral b :: Integer)
 where
  kb, mb, gb :: Double
  kb = 1024
  mb = 1024*kb
  gb = 1024*mb
  b' = fromIntegral b :: Double

-- | Runs an 'IO' action and returns a triple of the time it consumed (in sec),
-- the number of bytes it allocated, and the result it returned.
timeIt :: MonadIO m => m a -> m (Double, Int64, a)
timeIt action = do
  t0 <- liftIO getCurrentTime
  a0 <- liftIO getAllocationCounter
  ret <- action
  a1 <- liftIO getAllocationCounter
  t1 <- liftIO getCurrentTime
  return (realToFrac $ t1 `diffUTCTime` t0, a0 - a1, ret)

-- | Runs an 'IO' action, reduces the result to normal form and returns a pair
-- of the time it consumed and the result it returned.
timeNF :: NFData a => IO a -> IO (Double, Int64, a)
timeNF action = do
  t0 <- getCurrentTime
  a0 <- getAllocationCounter
  ret <- evaluate . force =<< action
  a1 <- getAllocationCounter
  t1 <- getCurrentTime
  return (realToFrac $ t1 `diffUTCTime` t0, a0 - a1, ret)

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
