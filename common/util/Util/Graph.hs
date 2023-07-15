{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.Graph
  ( postorder
  ) where

import Data.Hashable
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

-- | Post-order traversal of a graph: guarantees that the dependents
-- of a node occur before it in the result list, while as far as
-- possible retaining the original order of the nodes.
postorder
  :: (Hashable vertex, Eq vertex)
  => [node]
    -- ^ Nodes in the order of traversal
  -> (node -> vertex)
    -- ^ Extract a hashable vertex from the node
  -> (node -> [vertex])
    -- ^ Out-edges from a node. Vertices that aren't in the set of
    -- nodes are ignored.
  -> [node]
    -- ^ Result of post-order traversal
postorder nodes vert out = go HashSet.empty nodes (\_ -> [])
  where
  m = HashMap.fromList [ (vert n, n) | n <- nodes ]

  go seen [] cont = cont seen
  go seen (n : nodes) cont
    | v `HashSet.member` seen = go seen nodes cont
    | otherwise = go (HashSet.insert v seen) deps
       (\seen -> n : go seen nodes cont)
    where
      v = vert n
      deps = [ n | v <- out n, Just n <- [HashMap.lookup v m] ]
