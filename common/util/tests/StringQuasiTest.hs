{-# LANGUAGE QuasiQuotes #-}
module StringQuasiTest (main, tests) where

import Data.List

import Test.HUnit
import TestRunner

import Util.String.Quasi

main :: IO ()
main = testRunner tests

tests :: Test
tests = TestLabel "StringQuasiTest" $ TestList
  [ TestLabel "util-string-quasi" . TestCase $ do
      let str = [s|
this is a
multi-line string literal with \no escaping\|]
      assertBool "util-string-quasi" ("\\no escaping\\" `isSuffixOf` str)
  ]
