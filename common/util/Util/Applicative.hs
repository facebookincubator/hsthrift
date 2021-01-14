-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Applicative
  ( concatMapM
  ) where

concatMapM :: (Applicative f) => (a -> f [b]) -> [a] -> f [b]
concatMapM f = fmap concat . traverse f
