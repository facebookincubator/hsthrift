{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module MapTest where

import Map.Types
import Thrift.Protocol.JSON

import Test.HUnit
import TestRunner
import qualified Data.Map.Strict as Map

-- Make sure map serialization works

struct :: X
struct = X
  { intMap = Map.fromList [(1, "hello"), (2, "world")]
  , otherMap = Map.empty
  , optMap = Just $ Map.fromList [(1,1), (2,2)]
  , nestedMap = Map.fromList [(1, Map.fromList [(1,2), (3,4)])]
  , listMap = [Map.empty, Map.fromList [(0,0)]]
  , structMap = mempty
  , newtypeMap = mempty
  }

roundTripMapTest :: Test
roundTripMapTest = TestLabel "serialize map" $
  Right struct ~=? deserializeJSON (serializeJSON struct)

main :: IO ()
main = testRunner $ TestList
  [ roundTripMapTest ]
