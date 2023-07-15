{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
module StringQuasiTest (main) where

import Data.List

import Test.HUnit
import TestRunner

import Util.String.Quasi

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "util-string-quasi" . TestCase $ do
      let str = [s|
this is a
multi-line string literal with \no escaping\|]
      assertBool "util-string-quasi" ("\\no escaping\\" `isSuffixOf` str)
  ]
