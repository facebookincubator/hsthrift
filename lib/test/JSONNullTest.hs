module JSONNullTest where

import Test.HUnit
import TestRunner

import Thrift.Protocol.JSON

import HsTest.Types

nullParsingTest :: Test
nullParsingTest = TestLabel "JSON with Null" $ TestCase $
  assertEqual "parse null" (Right $ Foo 999 0) $ deserializeJSON
    "{\"bar\":999,\"baz\":null}"

tests :: Test
tests = TestLabel "JSONNullTest" $ TestList
  [ nullParsingTest
  ]

main :: IO ()
main = testRunner tests
