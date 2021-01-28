-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Defer (
  Defer, Embed(..), now, later, immediately, lift
) where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.STM
import Control.Monad.State.Strict

-- | Used to link an STM transaction and deferred IO actions to be run when that
-- transaction has been completed.
newtype Defer io stm a = Defer (StateT (io ()) stm a)
  deriving(
    Functor, Applicative, Alternative, Monad,
    MonadTrans, MonadThrow, MonadCatch)

-- | Run a given STM transaction inside a 'Defer'.
now :: Monad stm => stm a -> Defer io stm a
now = lift

-- | Used inside 'immediately' actions to append deferred IO actions to run
-- after the STM transaction has been completed.
later :: (Applicative io, Monad stm) => io () -> Defer io stm ()
later io = Defer $ modify' (*> io)

class Embed m1 m2 where
  embed :: m1 a -> m2 a

instance Embed STM IO where
  embed = atomically

-- | Run an STM transaction and then its deferred IO action.
immediately :: (Monad io, Monad stm, Embed stm io) => Defer io stm a -> io a
immediately (Defer action) = do
  (x,deferred) <- embed $ runStateT action $ return ()
  deferred
  return x
