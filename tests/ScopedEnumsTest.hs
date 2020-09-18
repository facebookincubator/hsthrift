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

module ScopedEnumsTest where

import Test.HUnit
import TestRunner

import ScopedEnums.Types
import Thrift.Protocol (fromThriftEnum)

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "X.A" $ fromThriftEnum X_A ~?= 1
  , TestLabel "Y.A" $ fromThriftEnum Y_A ~?= -1
  , TestLabel "Z.A" $ fromThriftEnum A_A ~?= 9
  , TestLabel "x" $ x ~?= X_A
  , TestLabel "y" $ y ~?= Y_A
  , TestLabel "z" $ z ~?= A_A
  ]
