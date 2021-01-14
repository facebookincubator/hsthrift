-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Function
  ( compose
  ) where

-- | Composes a list of endofunctions.
compose :: [a -> a] -> a -> a
compose = foldr (.) id
