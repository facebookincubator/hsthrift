{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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
