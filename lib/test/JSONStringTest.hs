-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TypeApplications #-}
module JSONStringTest where

import Thrift.Binary.Parser
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Text (Text)
import Test.HUnit
import TestRunner

import qualified Thrift.Protocol as Protocol
import Thrift.Protocol.JSON

unicodeParsingTest :: Test
unicodeParsingTest = TestLabel "json string paring" $ TestCase $
  assertEqual "parse string" (Right "\27979\35797") $
    parseText "\"\\u00e6\\u00b5\\u008b\\u00e8\\u00af\\u0095\""

parseText :: ByteString -> Either String Text
parseText = parse (Protocol.parseText @JSON Proxy)

expectParseError :: String -> ByteString -> Test
expectParseError name input = TestLabel name $ TestCase $
  case parseText input of
    Left{} -> return ()
    Right{} -> assertFailure "Should fail"

main :: IO ()
main = testRunner $ TestList
  [ unicodeParsingTest
  , expectParseError "Incomplete String" "\"xxx"
  , expectParseError "Incomplete Escape" "\"xxx\\\""
  , expectParseError "Incorrect Escape"  "\"\\x\""
  , expectParseError "Incorrect Unicode" "\"\\uwxyz\""
  , expectParseError "Incomplete Unicode" "\"\\u00\""
  ]
