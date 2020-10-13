--
-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements. See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership. The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- License); you may not use this file except in compliance
-- with the License. You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing,
-- software distributed under the License is distributed on an
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied. See the License for the
-- specific language governing permissions and limitations
-- under the License.
--

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
