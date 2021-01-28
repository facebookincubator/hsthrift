-- Copyright (c) Facebook, Inc. and its affiliates.

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
