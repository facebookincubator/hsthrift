{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
module Control.Trace.Core (
  Tracer (..),
  MonadTrace (..),
  MonadMaskInstance (..),
  logMsg,
  traceMsg,
  traceIf,
  Contravariant,
  (>$<),
) where

import Control.Monad (when)
import Control.Monad.Catch (
  ExitCase (..),
  MonadCatch,
  MonadMask (generalBracket),
  MonadThrow,
 )
import Control.Monad.IO.Class (
  MonadIO (..),
 )
import Data.Coerce
import Data.Functor.Contravariant (
  Contravariant (contramap),
  (>$<),
 )
import GHC.Stack (
  HasCallStack,
  withFrozenCallStack,
 )

-- | A contravariant tracing abstraction
data Tracer msg = Tracer
  { -- | Log a message
    logMsg_ :: msg -> IO ()
  , -- | Starts a trace and returns an action to end it
    traceMsg_
      :: forall a. HasCallStack => msg -> IO (ExitCase a -> IO ())
  }

logMsg :: (HasCallStack, MonadIO m) => Tracer msg -> msg -> m ()
logMsg logger msg = withFrozenCallStack $ liftIO $ logMsg_ logger msg

traceMsg ::
  (HasCallStack, MonadTrace m) => Tracer msg -> msg -> m a -> m a
traceMsg logger msg act = withFrozenCallStack $
  bracketM (traceMsg_ logger msg) id (const act)

instance Contravariant Tracer where
  contramap f (Tracer logf traceF) = Tracer (logf . f) (traceF . f)

instance Monoid (Tracer msg) where
  mempty = Tracer (\_ -> pure ()) (const $ pure $ const $ pure ())

instance Semigroup (Tracer msg) where
  l1 <> l2 =
    Tracer
      { logMsg_ = \m -> logMsg_ l1 m *> logMsg_ l2 m
      , traceMsg_ = \msg -> do
        end1 <- traceMsg_ l1 msg
        end2 <- traceMsg_ l2 msg
        return (\res -> end2 res >> end1 res)
      }

--------------------------------------------------------------------------------
-- useful combinators

-- | Gate every trace behind a condition
traceIf :: forall msg. IO Bool -> Tracer msg -> Tracer msg
traceIf cond tracer =
  let
    logMsg' msg = do
      value <- cond
      when value $ logMsg_ tracer msg
    traceMsg' :: msg -> IO (ExitCase b -> IO ())
    traceMsg' msg = do
      value <- cond
      if value
        then traceMsg_ tracer msg
        else pure $ pure $ pure ()
  in Tracer logMsg' traceMsg'

--------------------------------------------------------------------------------
-- A Monad for 'bracket'

class MonadIO m => MonadTrace m where
  bracketM :: IO a -> (a -> ExitCase b -> IO ()) -> (a -> m b) -> m b

-- deriving via (MonadMaskInstance IO) instance MonadTrace IO
instance MonadTrace IO where
  bracketM
    :: forall a b . IO a -> (a -> ExitCase b -> IO ()) -> (a -> IO b) -> IO b
  bracketM = coerce (bracketM @(MonadMaskInstance IO) @a @b)

-- | Deriving 'MonadTrace' via 'MonadMask'
newtype MonadMaskInstance m a = MonadMaskInstance (m a)
  deriving
    (Applicative, Functor, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow)

instance (MonadIO m, MonadMask m) => MonadTrace (MonadMaskInstance m) where
  bracketM acquire release =
    fmap fst . generalBracket (liftIO acquire) ((liftIO .) . release)
