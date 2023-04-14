-- Copyright (c) Facebook, Inc. and its affiliates.

module StreamTest (main) where

import Control.Concurrent
import Data.IORef
import Test.HUnit
import TestRunner

import Facebook.Init
import Util.TimeSec

import Control.Concurrent.Stream
import Data.List

-- Run a worker that sleeps for 1 second over 10 elements.
-- Using 5 workers means we should finish in less than 10 seconds.
timingTest :: Bool -> Test
timingTest bound = TestLabel "timing" $ TestCase $ do
  s <- now
  count <- newIORef (0::Int)
  let
    producer add = mapM_ add [1..(10::Int)]
    sleepy _ = do threadDelay 1000000; atomicModifyIORef' count (\i -> (i+1,()))
  (if bound then streamBound else stream) 5 producer sleepy
  e <- now
  let (TimeSpan d) = timeDiff e s
  assertBool "faster than sequential" (d < 5)
  final <- readIORef count
  assertEqual "all done" 10 final

testMapConcurrently_unordered :: Test
testMapConcurrently_unordered =
  TestLabel "mapConcurrently_unordered" $ TestCase $ do
    let input = [0..9]
    let expected = [1..10]
    startTime <- now
    output <- mapConcurrently_unordered
      10 (\x -> do threadDelay 1000000; pure (x + 1 :: Int)) input
    endTime <- now
    assertBool "Is faster than sequential"
      (toSeconds (timeDiff endTime startTime) < 10)
    assertBool "Received the expected output"
      (expected == sort output)

testMapForConcurrentlyEquivalent_unordered :: Test
testMapForConcurrentlyEquivalent_unordered =
  TestLabel
    "mapConcurrently_unordered==forConcurrently_unordered" $ TestCase $ do
      let input = [0..9]
      mapOut <- mapConcurrently_unordered 10 (\x -> pure $ x + (1 :: Int)) input
      forOut <- forConcurrently_unordered 10 input (\x -> pure $ x + (1 :: Int))
      assertBool
        "mapConcurrently_unordered and forConcurrently_unordered are equivalent"
        (sort mapOut == sort forOut)


main :: IO ()
main = withFacebookUnitTest $ testRunner $ TestList
  [ timingTest True
  , timingTest False
  , testMapConcurrently_unordered
  , testMapForConcurrentlyEquivalent_unordered
  ]
