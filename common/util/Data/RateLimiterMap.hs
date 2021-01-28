-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
module Data.RateLimiterMap
  ( -- * RateLimiterMap
    RateLimiterMap
  , newRateLimiterMap
  , newRateLimiterMapWithKeyPreprocessor
  , updateRateLimiterMapConfig
  , rateLimitFilter
  , whenAllowed
    -- * Types
  , Allowed(..)
  , SampleWeight
    -- * Primitives
  , isAllowed
  ) where

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Atomics

import Data.MovingAverageRateLimiter

type Count = Int
type SampleWeight = Int

-- | A 'RateLimiterMap' implements per-key rate limiting using
-- 'MovingAverageRateLimiter'. It is intentionally kept abstract.
data RateLimiterMap k = RateLimiterMap
  { rlmRef    :: {-# UNPACK #-} !(IORef (HashMap k (IORef RateLimitWithCount)))
  , rlmConfig :: {-# UNPACK #-} !(IORef RateLimiterConfig)
  , rlmKeyPreprocessor :: !(k -> IO k)
  }

-- Essentially a more strict and unboxed tuple.
data RateLimitWithCount = RateLimitWithCount {-# UNPACK #-} !Count !RateLimiter

-- | Indicates whether key is rate-limited.
-- 'Allowed' carries a weight, which is the number of calls to
-- 'isAllowed' since the last time 'Allowed' was returned.
data Allowed = NotAllowed | Allowed {-# UNPACK #-} !SampleWeight

-- | Create a new RateLimiterMap.
newRateLimiterMap
  :: Double                 -- ^ QPS Limit
  -> Double                 -- ^ Decay half-life, in seconds
  -> IO (RateLimiterMap k)
newRateLimiterMap lim hl = newRateLimiterMapWithKeyPreprocessor lim hl return

-- | Create a new RateLimiterMap with a preprocess provided, this preprocessor
-- is invoked before a key is inserted in the map. This may be needed in certain
-- scenarios when certain transformations need to be done on the keys before
-- they are inserted.
newRateLimiterMapWithKeyPreprocessor
  :: Double                 -- ^ QPS Limit
  -> Double                 -- ^ Decay half-life, in seconds
  -> (k -> IO k)      -- ^ Key preprocessor
  -> IO (RateLimiterMap k)
newRateLimiterMapWithKeyPreprocessor lim hl keyPreprocessor = do
  ref <- newIORef HashMap.empty
  cfgRef <- newIORef $ mkRateLimiterConfig lim hl
  return $ RateLimiterMap ref cfgRef keyPreprocessor

-- | Change the configuration of a 'RateLimiterMap'.
-- This DOES NOT change the internal state of individual limiters.
-- It only changes the qps limit and decay rate that will be applied
-- to future rate-limit checks.
updateRateLimiterMapConfig
  :: RateLimiterMap k
  -> (RateLimiterConfig -> RateLimiterConfig)
  -> IO ()
updateRateLimiterMapConfig RateLimiterMap{..} f =
  atomicModifyIORefCAS rlmConfig (\ c -> (f c, ()))

-- | Rate-limit an action.
-- This is the preferred means of using a 'RateLimiterMap'.
whenAllowed
  :: (Eq k, Hashable k)
  => RateLimiterMap k
  -> k
  -> (SampleWeight -> IO ()) -- ^ If allowed, run this action
  -> IO ()
whenAllowed rlm k f = do
  allowed <- isAllowed rlm k
  case allowed of
    NotAllowed -> return ()
    Allowed sw -> f sw

-- | Filter association list by rate limiter.
-- Return all values whose keys pass the rate limiter.
rateLimitFilter
  :: (Eq k, Hashable k)
  => RateLimiterMap k
  -> [(k, v)]
  -> IO [(SampleWeight, v)]
rateLimitFilter rlm = go
  where go []            = return []
        go ((k, v):kvs) = do
          allowed <- isAllowed rlm k
          case allowed of
            NotAllowed ->                 go kvs
            Allowed sw -> ((sw, v) :) <$> go kvs

-- | Determine whether a new action is allowed by the 'RateLimiterMap'.
-- This check is *not* idempotent (it bumps the rate-limit counter).
-- The preferred means to rate-limit an action is with 'whenAllowed'.
isAllowed :: (Eq k, Hashable k) => RateLimiterMap k -> k -> IO Allowed
isAllowed RateLimiterMap{..} k = do
  mbRL <- HashMap.lookup k <$> readIORef rlmRef
  cfg <- readIORef rlmConfig
  case mbRL of
    Just ref -> allowCheck cfg ref
    Nothing -> do
      -- Key hasn't been seen, create a new IORef for it.
      newRef <- newIORef $ RateLimitWithCount 0 mkRateLimiter
      newKey <- rlmKeyPreprocessor k
      -- Associate the IORef with the key. If its been added by
      -- another thread, return that IORef instead.
      newRef' <- atomicModifyIORefCAS rlmRef $ addOrReturnExisting newKey newRef
      allowCheck cfg newRef'

-- ----------------------------------------------------------------------------
-- Internals

-- | Run 'mutate' with the current time.
allowCheck :: RateLimiterConfig -> IORef RateLimitWithCount -> IO Allowed
allowCheck cfg rlRef = do
  t <- realToFrac <$> getPOSIXTime
  atomicModifyIORefCAS rlRef (mutate cfg t)

-- | Wrapper around MovingAverageRateLimiter's 'allow' which also counts
-- how many times the check has been performed. This count is used as the
-- sample weight whenever 'Allowed' is returned.
mutate
  :: RateLimiterConfig
  -> Double             -- ^ current timestamp
  -> RateLimitWithCount
  -> (RateLimitWithCount, Allowed)
mutate cfg t (RateLimitWithCount c rl) =
  case allow cfg rl t of
    (True , rl') -> (RateLimitWithCount 0     rl', Allowed (c+1))
    (False, rl') -> (RateLimitWithCount (c+1) rl', NotAllowed)

-- | Meant to be called inside atomicModifyIORef. Attempts to insert into
-- the HashMap, but if the key is already set, returns the existing value
-- instead. This way several threads can race-to-insert, one will win,
-- and the rest will get a copy of the winner's value.
addOrReturnExisting
  :: (Eq k, Hashable k)
  => k
  -> a
  -> HashMap k a
  -> (HashMap k a, a)
addOrReturnExisting k v m =
  case HashMap.lookup k m of
    Just v' -> (m, v')
    Nothing -> (HashMap.insert k v m, v)
