-- Copyright (c) Facebook, Inc. and its affiliates.

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
