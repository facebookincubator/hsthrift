-- Copyright (c) Facebook, Inc. and its affiliates.

module MD5Test (main) where

import Test.HUnit
import TestRunner

import Util.MD5

tests :: Test
tests = TestList
  [ TestLabel "md5Test" $ TestCase $
      assertEqual "md5Test" (md5 "wibble") "50eccc6e2b0d307d5e8a40fb296f6171" ]

main :: IO ()
main = testRunner tests
