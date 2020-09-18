module TestRunner where

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit

testRunner :: Test -> IO ()
testRunner t = hspec (fromHUnitTest t)
