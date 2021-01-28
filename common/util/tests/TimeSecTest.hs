-- Copyright (c) Facebook, Inc. and its affiliates.

module TimeSecTest (main) where

import Test.HUnit
import TestRunner

import Data.Aeson

import Util.TimeSec

jsonTest :: Test
jsonTest = TestLabel "Aeson instances" $ TestCase $ do
  assertEqual "toJSON" (Number 123) (toJSON $ Time 123)
  assertEqual "fromJSON" (eitherDecode "123")
    (timeInSeconds <$> eitherDecode "123")

main :: IO ()
main = testRunner $ TestList
  [ jsonTest
  ]
