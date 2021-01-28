-- Copyright (c) Facebook, Inc. and its affiliates.

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
