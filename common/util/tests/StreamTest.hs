-- Copyright (c) Facebook, Inc. and its affiliates.

module StreamTest (main) where

import Control.Concurrent
import Data.IORef
import Test.HUnit
import TestRunner

import Facebook.Init
import Util.TimeSec

import Control.Concurrent.Stream

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

main :: IO ()
main = withFacebookUnitTest $ testRunner $ TestList
  [ timingTest True
  , timingTest False
  ]
