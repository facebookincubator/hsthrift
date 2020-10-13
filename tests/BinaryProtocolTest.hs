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

module BinaryProtocolTest where

import Data.ByteString (ByteString)
import Test.HUnit
import TestRunner
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as VectorStorable

import Foo.Types
import Thrift.Protocol.Binary

foo :: Foo
foo = Foo (Just 10) True (Bar 99 "xxx") 3 (Vector.fromList [1,2,3])
          (VectorStorable.fromList [1,2,3])

cereal :: ByteString
cereal =
  "\n\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\STX\NUL\STX\SOH\f\NUL\ETX\b\255\255\NUL\NUL\NULc\v\NUL\SOH\NUL\NUL\NUL\ETXxxx\NUL\n\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\SI\NUL\ENQ\n\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\SI\NUL\ACK\n\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\NUL"

binaryTest :: Test
binaryTest = TestLabel "binaryTest" $ serializeBinary foo ~?= cereal

main :: IO ()
main = testRunner $ TestList
  [ binaryTest ]
