module TimeSecTest (main, tests) where

import Test.HUnit
import TestRunner

import Data.Aeson

import Util.TimeSec

jsonTest :: Test
jsonTest = TestLabel "Aeson instances" $ TestCase $ do
  assertEqual "toJSON" (Number 123) (toJSON $ Time 123)
  assertEqual "fromJSON" (eitherDecode "123")
    (timeInSeconds <$> eitherDecode "123")

tests :: Test
tests = TestLabel "TimeSecTest" $ TestList
  [ jsonTest
  ]

main :: IO ()
main = testRunner tests
