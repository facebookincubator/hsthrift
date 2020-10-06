-- Copyright (c) 2014, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

module Util.Applicative
  ( concatMapM
  ) where

concatMapM :: (Applicative f) => (a -> f [b]) -> [a] -> f [b]
concatMapM f = fmap concat . traverse f
