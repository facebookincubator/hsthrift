-- Copyright (c) Facebook, Inc. and its affiliates.

module JSONNullTest (main) where

import Test.HUnit
import TestRunner

import Thrift.Protocol.JSON

import HsTest.Types

nullParsingTest :: Test
nullParsingTest = TestLabel "JSON with Null" $ TestCase $
  assertEqual "parse null" (Right $ Foo 999 0) $ deserializeJSON
    "{\"bar\":999,\"baz\":null}"

main :: IO ()
main = testRunner $ TestList
  [ nullParsingTest
  ]
