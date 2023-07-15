{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module InteractionsTest where

import Test.HUnit
import TestRunner

import Thrift.Compiler.Parser

srcFile :: String
srcFile = unlines
  [ "interaction MyInteraction {"
  , "  void foo();"
  , "  i64 bar();"
  , "}"
  , ""
  , "service MyService {"
  , "  void baz();"
  , "  performs MyInteraction;"
  ,"}"
  ]

interactionTest :: Test
interactionTest = TestLabel "Interaction Test" $ TestCase $ do
  let file = "foo_bar"
  case snd <$> runParser parseThrift (file ++ ".thrift") srcFile of
    Right [ D_Interaction {},  D_Service {}] -> return ()
    _ -> assertFailure "not equal"

main :: IO ()
main = testRunner interactionTest
