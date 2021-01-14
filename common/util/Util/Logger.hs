-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Logger
  ( ActionLog(..)
  , loggingAction ) where

import Control.Exception
import Data.Int

import Util.Control.Exception (swallow)
import Util.Timing (timeIt)

-- | Logs which can log success, failure and time elapsed
class ActionLog l where
  successLog :: l
  failureLog :: SomeException -> l
  timeLog :: Double -> l
  allocLog :: Int64 -> l

-- | Run an action logging success, failure, time elapsed and data about
-- result.
loggingAction
  :: (ActionLog l, Monoid l)
  => (l -> IO ())
     -- ^ How to write the log. Typically calls the `runLog` for your
     -- `Logger` instance, and it can augment `l` with additional
     -- information known about this action.
  -> (a -> l)
     -- ^ What to log based on the result of the action
  -> IO a
     -- ^ The action to run
  -> IO a
loggingAction log res io =
  mask $ \restore -> do
    (time,alloc,result) <- timeIt $ try $ restore io
    let
      logOutcome o =
        swallow $ log $ timeLog time `mappend` allocLog alloc `mappend` o
    case result of
      Right x -> do
        logOutcome $ successLog `mappend` res x
        return x
      Left ex -> do
        logOutcome $ failureLog ex
        throwIO ex
