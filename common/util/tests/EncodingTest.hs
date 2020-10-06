module EncodingTest (main) where

import EncodingLib (regexMatchWord)

import Facebook.Init

import Test.HUnit
import TestRunner

regexTest :: Assertion
regexTest = assertBool "regex match test" $ regexMatchWord "lunedi"

main :: IO ()
main = withFacebookUnitTest $
  testRunner $ TestList
    [ TestLabel "regexTest" $ TestCase regexTest
    ]
