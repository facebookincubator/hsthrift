-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Reader
  ( catchR, catchR_
  , bracketR, bracketR_
  , withRetry
  ) where

import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- | Same as 'catch' from 'Control.Exception', but lifted into a ReaderT
catchR
  :: Exception e
  => ReaderT r IO a
  -> (e -> ReaderT r IO a)
  -> ReaderT r IO a
catchR (ReaderT action) handler = ReaderT $ \env ->
  action env `catch` \e -> runReaderT (handler e) env

-- | Same as 'catchR', but handler doesn't take an argument
catchR_
  :: ReaderT r IO a
  -> ReaderT r IO a
  -> ReaderT r IO a
catchR_ action handler = action `catchR` \SomeException{} -> handler

-- | Same as 'bracket' from 'Control.Exception', but lifted into a ReaderT
bracketR
  :: ReaderT r IO a        -- ^ before
  -> (a -> ReaderT r IO b) -- ^ after
  -> (a -> ReaderT r IO c) -- ^ computation
  -> ReaderT r IO c
bracketR before after thing = ReaderT $ \env ->
  bracket (runReaderT before env)
          (flip runReaderT env . after)
          (flip runReaderT env . thing)

-- | Same as 'bracket_' from 'Control.Exception', but lifted into a ReaderT
bracketR_
  :: ReaderT r IO a -- ^ before
  -> ReaderT r IO b -- ^ after
  -> ReaderT r IO c -- ^ computation
  -> ReaderT r IO c
bracketR_ before after thing =
  bracketR before (const after) (const thing)

-- | Retries the given computation up to n times if it fails.
-- WARNING: only use this with actions that have NO SIDE EFFECTS on failure
withRetry :: Int -> ReaderT r IO a -> ReaderT r IO a
withRetry n action = foldl catchR action $ replicate n $
  \e@SomeException{} -> do lift $ print e ; action
