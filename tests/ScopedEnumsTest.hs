{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module ScopedEnumsTest where

import Test.HUnit
import TestRunner

import ScopedEnums.Types
import Thrift.Protocol (fromThriftEnum)

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "X.A" $ fromThriftEnum X_A ~?= 1
  , TestLabel "Y.A" $ fromThriftEnum Y_A ~?= -1
  , TestLabel "Z.A" $ fromThriftEnum A_A ~?= 9
  , TestLabel "x" $ x ~?= X_A
  , TestLabel "y" $ y ~?= Y_A
  , TestLabel "z" $ z ~?= A_A
  ]
