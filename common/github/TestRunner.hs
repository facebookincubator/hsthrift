-- Copyright (c) Facebook, Inc. and its affiliates.

module TestRunner (module TestRunner) where

import Test.HUnit
import Test.HUnit.Text

testRunner :: Test -> IO ()
testRunner = runTestTTAndExit
