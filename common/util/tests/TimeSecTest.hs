{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module TimeSecTest (main) where

import Test.HUnit
import TestRunner

import Data.Aeson

import Util.TimeSec

jsonTest :: Test
jsonTest = TestLabel "Aeson instances" $ TestCase $ do
  assertEqual "toJSON" (Number 123) (toJSON $ Time 123)
  assertEqual "fromJSON" (eitherDecode "123")
    (timeInSeconds <$> eitherDecode "123")

main :: IO ()
main = testRunner $ TestList
  [ jsonTest
  ]
