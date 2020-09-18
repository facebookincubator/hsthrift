module EncodingTest (main, tests) where

import EncodingLib (regexMatchWord)

import Facebook.Init

import Test.HUnit
import TestRunner

regexTest :: Assertion
regexTest = assertBool "regex match test" $ regexMatchWord "lunedi"

tests :: Test
tests = TestLabel "EncodingTest" $ TestList
    [ TestLabel "regexTest" $ TestCase regexTest
    ]

main :: IO ()
main = withFacebookUnitTest $
  testRunner tests
