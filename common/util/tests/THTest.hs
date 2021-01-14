-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TemplateHaskell #-}

module THTest (main) where

import Test.HUnit
import TestRunner
import Language.Haskell.TH
import Util.MD5

tests :: Test
tests = TestList
  [ TestLabel "th test" $ TestCase $
      assertEqual "th test" (md5 "dupa") $(runQ [| md5 "dupa" |]) ]

main :: IO ()
main = testRunner tests
