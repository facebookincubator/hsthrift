-- Copyright (c) Facebook, Inc. and its affiliates.

module RegexTest (main) where

import Test.HUnit
import TestRunner

import qualified Data.Text as Text
import Util.Regex

emptyRegexTest :: Test
emptyRegexTest = TestLabel "emptyMatch" $ TestCase $
  assertEqual "s/a*//g" example $ substituteNoLimit example "Z*" "#"
  where
  example = "example"

stackOverflowTest :: Test
stackOverflowTest = TestLabel "stackOverflow" $ TestCase $
  assertEqual "s/[13579]/%d/g" expected $ substituteNoLimit input "[13579]" "%d"
  where
  n = 20000
  input = Text.replicate n "1234567890"
  expected = Text.replicate n "%d2%d4%d6%d8%d0"

main :: IO ()
main = testRunner $ TestList
  [ emptyRegexTest
  , stackOverflowTest
  ]
