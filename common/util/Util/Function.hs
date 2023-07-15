{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.Function
  ( compose
  ) where

-- | Composes a list of endofunctions.
compose :: [a -> a] -> a -> a
compose = foldr (.) id
