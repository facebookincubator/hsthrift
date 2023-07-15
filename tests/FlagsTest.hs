{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module FlagsTest where

import Test.HUnit
import TestRunner
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Flags.Types

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "intConst" $ intConst ~?= (100 :: Int)
  , TestLabel "mapConst" $
    mapConst ~?= HashMap.fromList [("a", 1), ("b", 2)]
  , TestLabel "setConst" $ setConst ~?= HashSet.fromList [1, 2, 3]
  ]
