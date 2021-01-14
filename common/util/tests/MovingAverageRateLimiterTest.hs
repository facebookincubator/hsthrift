-- Copyright (c) Facebook, Inc. and its affiliates.

module MovingAverageRateLimiterTest (main) where

import Data.List
import Test.HUnit
import TestRunner

import Data.MovingAverageRateLimiter

smoothRateLimiter :: Test
smoothRateLimiter = TestList
  [ TestLabel "Smooth" $ TestCase $ assertEqual "allow 1qps" 0 fails
  , TestLabel "Smooth" $ TestCase $ assertEqual "don't allow 2qps" False
                 (fst $ allow cfg rl' (fromIntegral sz))
  ]
  where
  sz = 50 :: Int
  cfg = mkRateLimiterConfig 1.0 1.0
  rl = mkRateLimiter
  go :: (RateLimiter, Int) -> Int -> (RateLimiter, Int)
  go (r, f) now =
    let (a, r') = allow cfg r (fromIntegral now)
    in (r', f + fromEnum (not a))
  (rl', fails) = foldl' go (rl, 0) [1..sz]

nonSmoothRateLimiter :: Test
nonSmoothRateLimiter = TestList
  [ TestLabel "NonSmooth" $ TestCase $ assertEqual "allow 2qps" 0 fails
  , TestLabel "NonSmooth" $ TestCase $ assertEqual "don't allow 3qps" False
                 (fst $ allow cfg rl' (fromIntegral sz))
  ]
  where
  sz = 1000 :: Int
  cfg = mkRateLimiterConfig 2.01 10.0
  rl = mkRateLimiter
  go :: (RateLimiter, Int) -> Int -> (RateLimiter, Int)
  go (r, f) now =
    let (a1, r1) = allow cfg r (fromIntegral now)
        (a2, r2) = allow cfg r1 (fromIntegral now)
    in (r2, f + fromEnum (not a1) + fromEnum (not a2))
  (rl', fails) = foldl' go (rl, 0) [1..sz]

observedRateTest :: Test
observedRateTest = TestList
    [ TestLabel "ObservedRate" $ TestCase $
        assertEqual "don't allow at 2" False a1
    , TestLabel "ObservedRate" $ TestCase $
        assertEqual "allow at 3" True a2
    ]
  where
  cfg = mkRateLimiterConfig 1.0 1.0
  rl = mkRateLimiter
  rl2 = observedRate rl 1.01 2.0
  (a1, rl3) = allow cfg rl2 2
  (a2, _) = allow cfg rl3 3

qpsConverges :: Test
qpsConverges = TestList
  [ TestLabel "constant" $ TestCase $ convergeTest 1.0 600.0 86400.0
      [ 0.0, 0.1 .. 86400.0 ]
  , TestLabel "skip6hours" $ TestCase $ convergeTest 2.0 60.0 129600.0 $
      [ 0.0, 0.1 .. 21600.0] ++ [ 43200.0, 43200.1 .. 86400 ]
  ]

convergeTest :: Double -> Double -> Double -> [Double] -> Assertion
convergeTest qps0 hl expected bumps = do
  putStrLn $ "Expected QPS: " ++ show expected
  putStrLn $ "Got: " ++ show count
  assertBool "converges on expected rate" closeEnough
  where
    cfg = mkRateLimiterConfig qps0 hl
    go (r, n) t =
      let (a, r') = allow cfg r t
      in (r', n + fromEnum a)
    (_, count) = foldl' go (mkRateLimiter, 0 :: Int) bumps
    closeEnough = abs (fromIntegral count - expected) / expected < 0.01

main :: IO ()
main = testRunner $ TestList
  [ smoothRateLimiter
  , nonSmoothRateLimiter
  , observedRateTest
  , qpsConverges
  ]
