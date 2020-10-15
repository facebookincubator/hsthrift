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

module InputCheckingTest where

import Data.Proxy
import Test.HUnit
import TestRunner

import Versions.Types

import Thrift.Protocol
import Thrift.Protocol.Binary
import Thrift.Protocol.Compact
import Thrift.Protocol.JSON

-- Serialize a value of type a and then attempt to parse it as type b
mkTest
  :: ( ThriftSerializable a
     , ThriftSerializable b
     , Eq b
     , Show b
     )
  => String
  -> a
  -> b
  -> TestWrapper
mkTest lbl x y = TestWrapper $ \prot prx ->
  TestLabel (lbl ++ " Compat [" ++ prot ++ "]") $ TestCase $ do
    let cereal = serializeGen prx x
    case deserializeGen prx cereal of
      Left msg -> error msg
      Right val -> assertEqual lbl y val

newtype TestWrapper =
  TestWrapper (forall p. Protocol p => String -> Proxy p -> Test)

main :: IO ()
main = testRunner $ TestList $ concatMap applyProxies
  [ mkTest "Type Change" (X1 999) (X2 "")
  , mkTest "Remove Union Alt" (U1_y "XXX") (U2_EMPTY)
  , mkTest "Add Union Alt" (U1_x 123) (U2_x 123)
  , mkTest "Add Field" (Y2 123) (Y1 123 "")
  , mkTest "Remove Field" (Y1 123 "xyz") (Y2 123)
  , mkTest "Add Alt in List" (L1 [U1_x 123]) (L2 [U2_x 123])
  , mkTest "Remove Alt in List" (L1 [U1_y "XXX"]) (L2 [U2_EMPTY])
  ]
  where
    applyProxies (TestWrapper t) =
      [ t "Binary" (Proxy :: Proxy Binary)
      , t "Compact" (Proxy :: Proxy Compact)
      , t "JSON" (Proxy :: Proxy JSON)
      ]
