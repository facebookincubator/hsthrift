-- Copyright 2014-present Facebook. All Rights Reserved.

{-# LANGUAGE TemplateHaskell #-}

module THTest (main, tests) where

import Test.HUnit
import TestRunner
import Language.Haskell.TH
import Util.MD5

tests :: Test
tests = TestLabel "THTest" $ TestList
  [ TestLabel "th test" $ TestCase $
      assertEqual "th test" (md5 "dupa") $(runQ [| md5 "dupa" |]) ]

main :: IO ()
main = testRunner tests
