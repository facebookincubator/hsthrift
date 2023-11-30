{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DerivingVia #-}
module Control.Trace.Core (
  Tracer (..),
  MonadTrace (..),
  MonadMaskInstance (..),
  logMsg,
  traceMsg,
  Contravariant,
  (>$<),
) where

import Control.Exception (
  Exception,
 )
import Control.Monad.Catch (
  ExitCase (..),
  MonadCatch,
  MonadMask (generalBracket),
  MonadThrow,
  try,
 )
import Control.Monad.IO.Class (
  MonadIO (..),
 )
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
    logMsg_ :: forall m. (HasCallStack, MonadTrace m) => msg -> m ()
  , -- | Trace the begin and end of a computation
    traceMsg_ :: forall a m. (HasCallStack, MonadTrace m) => msg -> m a -> m a
  }

-- Explicit record accessors to preserve call stacks

logMsg :: (HasCallStack, MonadTrace m) => Tracer msg -> msg -> m ()
logMsg logger msg = withFrozenCallStack $ logMsg_ logger msg

traceMsg :: (HasCallStack, MonadTrace m) => Tracer msg -> msg -> m a -> m a
traceMsg logger msg act = withFrozenCallStack $ traceMsg_ logger msg act

instance Contravariant Tracer where
  contramap f (Tracer logf traceF) = Tracer (logf . f) (traceF . f)

instance Monoid (Tracer msg) where
  mempty = Tracer (\_ -> pure ()) (const id)

instance Semigroup (Tracer msg) where
  l1 <> l2 =
    Tracer
      { logMsg_ = \m -> logMsg_ l1 m *> logMsg_ l2 m
      , traceMsg_ = \msg -> traceMsg_ l1 msg . traceMsg_ l2 msg
      }

-------------------------------------------------------------------------------
-- Exceptions

class MonadIO m => MonadTrace m where
  tryM :: Exception e => m a -> m (Either e a)
  bracketM :: IO a -> (a -> ExitCase b -> IO ()) -> (a -> m b) -> m b

deriving via (MonadMaskInstance IO) instance MonadTrace IO

-- | Deriving 'MonadTrace' via 'MonadMask'
newtype MonadMaskInstance m a = MonadMaskInstance (m a)
  deriving
    (Applicative, Functor, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow)

instance (MonadIO m, MonadMask m) => MonadTrace (MonadMaskInstance m) where
  tryM = try
  bracketM acquire release =
    fmap fst . generalBracket (liftIO acquire) ((liftIO .) . release)
