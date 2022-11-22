-- Copyright (c) Facebook, Inc. and its affiliates.

module PseudoThriftEnumTest where

import qualified Enum.Types as Enum
import Data.Proxy
import qualified Pseudoenum.Types as Pseudoenum
import Test.HUnit
import TestRunner
import Thrift.Protocol

perfectEnum :: Proxy Enum.PerfectEnum
perfectEnum = Proxy

perfectPseudoenum :: Proxy Pseudoenum.PerfectEnum
perfectPseudoenum = Proxy

sameFromThriftEnumTest :: Test
sameFromThriftEnumTest = TestLabel "same fromThriftEnum" $ TestCase $ do
  let
    enumValues = allValuesPlusUnknown perfectEnum fromThriftEnum
    pseudoenumValues = allValuesPlusUnknown perfectPseudoenum fromThriftEnum
  assertEqual "fromThriftEnum" enumValues pseudoenumValues

sameShowTest :: Test
sameShowTest = TestLabel "same show" $ TestCase $ do
  let
    enumValues = allValuesPlusUnknown perfectEnum show
    pseudoenumValues = allValuesPlusUnknown perfectPseudoenum show
  assertEqual "show" enumValues pseudoenumValues

allValuesPlusUnknown :: ThriftEnum a => Proxy a -> (a -> b) -> [b]
allValuesPlusUnknown proxy f = map f $ allThriftEnumValues ++ [unknownValue]
  where
    unknownValue = toThriftEnum $ (+1) $ maximum $
      map (fromThriftEnum . (`asProxyTypeOf` proxy)) allThriftEnumValues

main :: IO ()
main = testRunner $ TestList
  [ sameFromThriftEnumTest
  , sameShowTest
  ]
