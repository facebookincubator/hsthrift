-- Copyright (c) Facebook, Inc. and its affiliates.

-- @nolint
{-# LANGUAGE RecordWildCards #-}
import Data.Semigroup (Semigroup(..))
import Prelude ((++), Monoid(..))

instance Semigroup X where
  (<>) = mappend

instance Prelude.Monoid X where
  mempty = X [] []
  mappend X{..} (X y1 y2) = X (x_intList ++ y1) (x_stringList ++ y2)
