-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TypeApplications #-}
module EnumTest where

import Thrift.Binary.Parser
import qualified Data.ByteString as ByteString
import Data.Proxy
import Test.HUnit
import TestRunner

import Thrift.Protocol
import Thrift.Protocol.Binary

import Enum.Types

enumListTest :: Test
enumListTest = TestLabel "enum list" $ TestCase $
  assertEqual "allThriftEnumValues"
  [UnsortedEnum_A, UnsortedEnum_B, UnsortedEnum_C, UnsortedEnum_D,
   UnsortedEnum_E, UnsortedEnum_G]
  (allThriftEnumValues :: [UnsortedEnum])

ordTest :: Test
ordTest = TestLabel "Ord" $ TestCase $
  assertBool "Ord" $ UnsortedEnum_A < UnsortedEnum_E

perfectEnumTest :: Test
perfectEnumTest = TestLabel "perfect" $ TestCase $
  assertEqual "perfect enum"
  (map fromThriftEnum (allThriftEnumValues :: [PerfectEnum])) [0..3]

enumParseNoErrorTest :: Test
enumParseNoErrorTest = TestLabel "Parse No Error" $ TestCase $ do
  let
    rawI32 = ByteString.pack [0,0,0,8]
    result = parse (parseEnum @Binary @UnsortedEnum Proxy "UnsortedEnum") rawI32
  assertEqual "Parse No Error" (Right $ UnsortedEnum__UNKNOWN 8) result

-- Enum with hs.nounknown annotation will throw when enum value is unknown
enumParseErrorTest :: Test
enumParseErrorTest = TestLabel "Parse Error" $ TestCase $ do
  let
    rawI32 = ByteString.pack [0,0,0,8]
    result = parse
      (parseEnumNoUnknown @Binary @EnumWithNounknown Proxy "EnumWithNounknown")
      rawI32
    errorMsg = "ParseError \"parseEnum: not a valid identifier for thrift enum "
      ++ "'EnumWithNounknown': 8. Failed reading at byte position 4\""
  assertEqual "Parse Error" (Left errorMsg) result

toThriftEnumEitherTest :: Test
toThriftEnumEitherTest = TestLabel "toThriftEnumEither" $ TestCase $ do
  assertEqual "Right PerfectEnum"
    (toThriftEnumEither 0 :: Either String PerfectEnum) (Right PerfectEnum_W)
  assertEqual "Left String"
    (toThriftEnumEither 4 :: Either String PerfectEnum)
    (Left "toThriftEnumEither: not a valid identifier for enum PerfectEnum: 4")


main :: IO ()
main = testRunner $ TestList
  [ enumListTest
  , ordTest
  , perfectEnumTest
  , enumParseErrorTest
  , toThriftEnumEitherTest
  ]
