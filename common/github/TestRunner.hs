{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module TestRunner (module TestRunner) where

import Test.HUnit
import Test.HUnit.Text

testRunner :: Test -> IO ()
testRunner = runTestTTAndExit
