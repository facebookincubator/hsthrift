{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TemplateHaskell #-}

module THTest (main) where

import Test.HUnit
import TestRunner
import Language.Haskell.TH
import Util.MD5

tests :: Test
tests = TestList
  [ TestLabel "th test" $ TestCase $
      assertEqual "th test" (md5 "dupa") $(runQ [| md5 "dupa" |]) ]

main :: IO ()
main = testRunner tests
