-- Copyright (c) Facebook, Inc. and its affiliates.

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Util.AllocLimit
  ( limitAllocs
  , withAllocLimit
  , withSavedAllocLimit
  , withAllocCounter
  , handleAllExceptions
  ) where

import GHC.Conc
import Data.Int
import Control.Exception

-- | Impose an allocation limit on the given 'IO' action.  If the
-- action exceeds the limit, then 'Nothing' will be returned.
limitAllocs :: Int64 -> IO a -> IO (Maybe a)
limitAllocs limit action =
  handle errorHandler $ Just <$> withAllocLimit limit action
 where
  -- like handleAllExceptions but only handles AllocationLimitExceeded
  errorHandler :: AllocationLimitExceeded -> IO (Maybe a)
  errorHandler _ =
    handle errorHandler $ do
      allowInterrupt -- let any pending async exceptions throw
      return Nothing

-- | Impose an allocation limit on the given 'IO' action.  If the
-- action exceeds the limit, then 'AllocationLimitExceeded' will be
-- thrown. (Note: most uses should use 'limitAllocs' instead).
--
-- Note that if this function throws an exception
-- ('AllocationLimitExceeded' or otherwise), even though there will be
-- no /more/ 'AllocationLimitExceeded' exceptions, there might be
-- multiple 'AllocationLimitExceeded' exceptions already in flight.
-- This happens as follows: the first exception gets thrown, which
-- causes an exception handler to run, which triggers another
-- 'AllocationLimitExceeded' exception, which doesn't get thrown
-- immediately because the exception handler is implicitly masked, and
-- so on.
--
-- To deal with this we provide 'handleAllExceptions', which you
-- should wrap around 'withAllocLimit'.
--
-- Note: this function clobbers the allocation counter, so
-- e.g. `Util.Timing.timeIt` will give bogus results if the
-- computation being measured calls `withAllocLimit`. To avoid this,
-- use `withSavedAllocLimit` instead.
withAllocLimit :: Int64 -> IO a -> IO a
withAllocLimit limit =
  bracket_
    (do setAllocationCounter limit; enableAllocationLimit)
    disableAllocationLimit

-- | Like `withAllocLimit`, but does not clobber the allocation
-- counter.
withSavedAllocLimit :: Int64 -> IO a -> IO a
withSavedAllocLimit limit io = bracket set unset $ \_ -> io
  where
  set = do
    prev <- getAllocationCounter
    setAllocationCounter limit
    enableAllocationLimit
    return prev
  unset prev = do
    cur <- getAllocationCounter
    disableAllocationLimit
    setAllocationCounter (prev - (limit - cur))

-- | Sets the allocation counter for the given 'IO' action, then restores the
-- original counter value after the 'IO' action is done.
--
-- Note that as opposed to 'withAllocLimit' this function provides no guarantee
-- that if the set alloc counter is consumed, an 'AllocationLimitExceeded' will
-- be thrown, that depends on whether 'enableAllocationLimit' has been called
-- or not. If that guarantee is something you need, use 'withAllocLimit', but be
-- mindful of the fact that 'withAllocLimit' calls should not be nested inside
-- each other while 'withAllocCounter' calls can.
--
-- This function can be useful when there's a need to "isolate" certain
-- calls from the current allocation limit by setting a separate allocation
-- limit for them, then restoring the initial limit when done.
--
-- Once this is done: T30781590. We will be able to use the new primitive in
-- 'withAllocLimit' and deprecate 'withAllocCounter'
withAllocCounter :: Int64 -> IO a -> IO a
withAllocCounter counter action = do
  initialAllocCounter <- getAllocationCounter
  bracket_
    (setAllocationCounter counter)
    (setAllocationCounter initialAllocCounter)
    action

-- | Like 'Control.Exception.handle', but if there are pending async
-- exceptions, all are swallowed except for the last one, which is
-- passed to the handler.  This is mainly for wrapping around
-- 'withAllocLimit', to ensure that no 'AllocationLimitExceeded'
-- exceptions can leak out.
--
handleAllExceptions :: (SomeException -> IO a) -> IO a -> IO a
handleAllExceptions handler = handle errorHandler
  where
    errorHandler exn =
      handle errorHandler $ do
        allowInterrupt -- let any pending async exceptions throw
        handler exn
