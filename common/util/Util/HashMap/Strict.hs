{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.HashMap.Strict
  ( mapKeys
  ) where

import Control.Arrow (first)
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

-- | Transform a HashMap by applying a function to every key.
mapKeys :: (Eq b, Hashable b) => (a -> b) -> HashMap a v -> HashMap b v
mapKeys mapper =
  HashMap.fromList . map (first mapper) . HashMap.toList
