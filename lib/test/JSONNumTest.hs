-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TypeApplications #-}
module JSONNumTest where

import Thrift.Binary.Parser
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder
import Data.Proxy
import Test.HUnit
import TestRunner

import qualified Data.ByteString as B
import qualified Thrift.Protocol as Protocol
import Thrift.Protocol.JSON

serializationTests :: [(String, Double, ByteString)]
serializationTests =
  [ ( "Should handle a double appropiately"
    , 10.1
    , "10.1"
    )
  , ( "Should handle NaN appropiately"
    , 0/0
    , "\"NaN\""
    )
  ]

doubleGenTest :: (String, Double, ByteString) -> Test
doubleGenTest (testLabel, input, expected) = TestLabel testLabel $ TestCase $
  assertEqual "gen double" expected $
    genDouble input

genDouble :: Double -> ByteString
genDouble = toLazyByteString . Protocol.genDouble @JSON Proxy

parserTests :: [(String, B.ByteString, Double)]
parserTests =
  [ ( "Should parse a double correctly"
    , "10.1"
    , 10.1
    )
  , ( "Should parse NaN correctly"
    , "\"NaN\""
    , 0/0
    )
  ]

parsingGenTest :: (String, B.ByteString, Double) -> Test
parsingGenTest (testLabel, input, expected) = TestLabel testLabel $ TestCase $
  case parseDouble input of
    Right num -> if isNaN num
      then assertBool "parse NaN" $ isNaN expected
      else assertEqual "parse double" expected num
    Left _ -> assertFailure "failed to parse"

parseDouble :: B.ByteString -> Either String Double
parseDouble = parse (Protocol.parseDouble @JSON Proxy)

main :: IO ()
main = testRunner $ TestList $
  map doubleGenTest serializationTests ++
  map parsingGenTest parserTests
