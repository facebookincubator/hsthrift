module Main where

import qualified BinaryParserTest
import qualified ChannelTest
import qualified ClientTest
import qualified HeaderChannelTest
import qualified JSONNullTest
import qualified JSONNumTest
import qualified JSONStringTest

import TestRunner
import Test.HUnit

main :: IO ()
main = testRunner $ TestLabel "thrift-lib-tests" $ TestList
  [ BinaryParserTest.tests
  , ChannelTest.tests
  , ClientTest.tests
  -- , HeaderChannelTest.tests
  -- disabled ^^^ because I haven't yet configured my
  -- docker daemon to use ipv6, which seems required by
  -- this test.
  , JSONNullTest.tests
  , JSONNumTest.tests
  , JSONStringTest.tests
  ]

{- with HeaderChannelTest disabled, all tests pass. but the
   executable crashes, with:

free(): double free detected in tcache 2
Aborted (core dumped)

-}
