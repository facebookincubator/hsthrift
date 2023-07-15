{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.Applicative
  ( concatMapM
  ) where

concatMapM :: (Applicative f) => (a -> f [b]) -> [a] -> f [b]
concatMapM f = fmap concat . traverse f
