-- Copyright (c) Facebook, Inc. and its affiliates.

{- |
A standalone rate limiter that accepts some fluctuations in the traffic.

QPS_new = QPS_old * exp(-alpha * t) + n * alpha
where alpha is the decay factor, t is the time elapsed and n is the number of
new requests (that are allowed).

See also www/flib/social/ma_rate_limiter/MovingAverageRateLimiter.php
and fbcode/common/datastruct/MovingAverageRateLimiter.h
-}

module Data.MovingAverageRateLimiter
  ( RateLimiterConfig(qpsLimit, decayHalfLife)
  , mkRateLimiterConfig
  , RateLimiter(qps)
  , mkRateLimiter
  , updateConfig
  , observedRate
  , allow
  ) where

-- ----------------------------------------------------------------------------

data RateLimiterConfig = RateLimiterConfig
  { qpsLimit      :: {-# UNPACK #-} !Double  -- ^ Query per second limit
  , decayHalfLife :: {-# UNPACK #-} !Double
  -- ^ Time in seconds for the weight of past query to decay to half
  -- in moving average calculation
  , alpha         :: {-# UNPACK #-} !Double
  }

mkRateLimiterConfig
  :: Double  -- ^ query per second limit
  -> Double  -- ^ decay half life
  -> RateLimiterConfig
mkRateLimiterConfig ql d = RateLimiterConfig
  { qpsLimit = ql
  , decayHalfLife = d
  , alpha = log 2.0 / d
  }

updateConfig
  :: Double  -- ^ new query per second limit
  -> Double  -- ^ new decay half life
  -> RateLimiterConfig -> RateLimiterConfig
updateConfig ql d r = r
  { qpsLimit = ql
  , decayHalfLife = d
  , alpha = log 2.0 / d
  }

-- ----------------------------------------------------------------------------

data RateLimiter = RateLimiter
  { qps           :: {-# UNPACK #-} !Double
  , lastTimestamp :: {-# UNPACK #-} !Double  -- in sec
  }

mkRateLimiter :: RateLimiter
mkRateLimiter = RateLimiter
  { qps = 0
  , lastTimestamp = 0
  }

observedRate
  :: RateLimiter
  -> Double      -- ^ queries per second
  -> Double      -- ^ timestamp of the last query
  -> RateLimiter
observedRate r q t = r
  { qps = q
  , lastTimestamp = t
  }

allow
  :: RateLimiterConfig
  -> RateLimiter
  -> Double               -- ^ current timestamp
  -> (Bool, RateLimiter)  -- ^ should the query is allowed? and the new state
allow RateLimiterConfig{..} r@RateLimiter{..} t = (a, observedRate r q2 t)
  where
  timeDiff = t - lastTimestamp
  q1 = if timeDiff > 0 then qps * exp (- alpha * timeDiff) else qps
  (a, q2) | lastTimestamp == 0 = (True , qpsLimit * (1 - alpha) + alpha)
                                   -- See Note [First Bump]
          | q1 < qpsLimit      = (True , q1 + alpha)
          | otherwise          = (False, q1)

-- Note [First Bump]
-- If the lastTimestamp is zero, this is the first time a RateLimiter has been
-- bumped. In that case, always allow, then set the qps to the qpsLimit minus
-- enough room for (qpsLimit - 1) additional bumps. This means the RateLimiter
-- will allow qpsLimit bumps at time zero, then throttle at bump qpsLimit + 1.
-- Without this, time is required for the count to approach the decay rate,
-- resulting in an initial spike of allowed bumps. For example, a RateLimiter
-- configured to 1 qps with 600 s half-life would allow at least the first
-- 865 bumps before actually limiting.
