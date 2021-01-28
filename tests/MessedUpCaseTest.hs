-- Copyright (c) Facebook, Inc. and its affiliates.

module MessedUpCaseTest where

import Test.HUnit
import TestRunner

import MessedUpCase.Types

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "str const" $ str ~?= ""
  , TestLabel "int type" $ (1 :: Int) ~?= (1 :: IntType)
  , TestLabel "enums" $ TestCase $ assertBool "one /= two" $
    Numbers_one /= Numbers_two
  , TestLabel "struct" $ field (Foo 0) ~?= 0
  ]
