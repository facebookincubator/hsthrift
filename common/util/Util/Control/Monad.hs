{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.Control.Monad
  ( whenMonoid
  , whenDefault
  , whenMaybe
  , firstMLazy
  ) where

whenDefault :: Monad m => Bool -> a -> m a -> m a
whenDefault cond def monad =
  if cond then monad else return def

whenMaybe :: Monad m => Bool -> m (Maybe a) -> m (Maybe a)
whenMaybe b = whenDefault b Nothing

whenMonoid :: (Monad m, Monoid a) => Bool -> m a -> m a
whenMonoid b = whenDefault b mempty

-- | Performs the operations sequentially, and returns the first non-Nothing
-- value or throws the first exception immediately, whichever comes first.
--
-- Note: this serializes all the operations, only use this if you know
-- what you're doing.
--
firstMLazy :: (Foldable f, Monad m) => f (m (Maybe a)) -> m (Maybe a)
firstMLazy = foldr (\i j -> maybe j (pure . Just) =<< i) (pure Nothing)
