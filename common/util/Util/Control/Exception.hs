-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.Control.Exception
  ( -- * Catching all exceptions safely
    catchAll
  , handleAll
  , tryAll
    -- * Exception predicates
  , isSyncException
  , isAsyncException
    -- * Other utilities
  , throwLeftIO
  , throwLeftExceptionIO
  , tryBracket
  , tryFinally
  , onSomeException
  , afterwards
  , swallow
  , logExceptions
  ) where

#if __GLASGOW_HASKELL__ == 804
import Control.Exception ( SomeAsyncException(..) )
#endif
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Trans.Control

import Util.Log

-- | Catch all exceptions *except* asynchronous exceptions
-- (technically, children of 'SomeAsyncException').  Catching
-- asynchronous exceptions is almost never what you want to do: it can
-- result in ignoring 'ThreadKilled' which can lead to deadlock (see
-- <https://our.internmc.facebook.com/intern/diff/D4745709/
-- D4745709>).
--
-- Use this instead of the raw 'catch' when catching 'SomeException'.
--
catchAll :: MonadBaseControl IO m => m a -> (SomeException -> m a) -> m a
catchAll action handler =
  action `catch` \ex ->
    case fromException ex of
      Just (_ :: SomeAsyncException) -> throwIO ex
      Nothing -> handler ex

-- | The "try" version of 'catchAll'
tryAll :: MonadBaseControl IO m => m a -> m (Either SomeException a)
tryAll action = (Right <$> action) `catchAll` (return . Left)

-- | Flipped version of 'catchAll'
handleAll :: MonadBaseControl IO m => (SomeException -> m a) -> m a -> m a
handleAll = flip catchAll

throwLeftIO :: Exception e => Either e a -> IO a
throwLeftIO = throwLeftExceptionIO id

throwLeftExceptionIO :: Exception e => (a -> e) -> Either a b -> IO b
throwLeftExceptionIO mkEx e = either (throwIO . mkEx) pure e

-- | Detect 'SomeAsyncException' wrapped exceptions versus all others
isSyncException :: Exception e => e -> Bool
isSyncException e = case fromException (toException e) of
  Just (SomeAsyncException _) -> False
  Nothing -> True

-- | Detect 'SomeAsyncException' wrapped exceptions versus all others
isAsyncException :: Exception e => e -> Bool
isAsyncException = not . isSyncException

-- | A variant of 'bracket' where the release action also gets to see whether
-- the inner action succeeded or threw an exception.
tryBracket
  :: IO a                                   -- ^ run first
  -> (a -> Either SomeException b -> IO ()) -- ^ run finally
  -> (a -> IO b)                            -- ^ run in between
  -> IO b
tryBracket before after inner =
  mask $ \restore -> do
    a <- before
    r <- restore (inner a) `catch` \ex -> do
      after a (Left ex)
      throwIO ex
    _ <- after a (Right r)
    return r

-- | A variant of 'finally' where the final action also gets to see whether
-- the first action succeeded or threw an exception.
tryFinally
  :: IO a                              -- ^ run first
  -> (Either SomeException a -> IO ()) -- ^ run finally
  -> IO a
tryFinally inner after =
  mask $ \restore -> do
    r <- restore inner `catch` \ex -> do
      after (Left ex)
      throwIO ex
    _ <- after (Right r)
    return r

-- | Execute an action and invoke a function if it throws any exception. The
-- exception is then rethrown. Any exceptions from the function are ignored
-- (but logged).
onSomeException :: IO a -> (SomeException -> IO ()) -> IO a
onSomeException io f = io `catch` \exc -> do
  swallow $ f exc
  throwIO exc

-- | Execute an action and do something with its result even if it throws a
-- synchronous exception. Any exceptions from the function are ignored
-- (but logged).
afterwards :: IO a -> (Either SomeException a -> IO ()) -> IO a
afterwards io f = do
  r <- tryAll io
  swallow $ f r
  case r of
    Right result -> return result
    Left exc -> throwIO exc

-- | Execute an action and drop its result or any synchronous
-- exception it throws.  Exceptions are logged.
swallow :: IO a -> IO ()
swallow io = void io `catchAll` \exc ->
  logError $ "swallowing exception: " ++ show exc

-- | Log and rethrow all synchronous exceptions arising from an
-- IO computation.
logExceptions :: (String -> String) -> IO a -> IO a
logExceptions f io = io `onSomeException` (logError . f . show)
