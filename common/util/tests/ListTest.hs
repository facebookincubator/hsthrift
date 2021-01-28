-- Copyright (c) Facebook, Inc. and its affiliates.

module ListTest (main) where

import Test.QuickCheck
import Test.HUnit
import TestRunner
import Util.List

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "atMostChunksOf" $ TestCase $ do
      result <- quickCheckResult prop_atMostChunksOf
      case result of
        Success{} -> return ()
        _ -> assertFailure "failed"
  , TestLabel "chunkBy" $ TestCase $ do
      result <- quickCheckResult prop_chunkBy
      case result of
        Success{} -> return ()
        _ -> assertFailure "failed"
  ]

prop_atMostChunksOf :: Int -> [Int] -> Bool
prop_atMostChunksOf n xs =
  n <= 0 || null xs ||
  (  all ((<= n) . length) chunks
  && all (not . null) chunks
  && concat chunks == xs
  && length chunks <= (length xs `div` n + 1)
  )
  where
  chunks = atMostChunksOf n xs

prop_chunkBy :: Int -> [Int] -> Bool
prop_chunkBy n xs =
  n <= 0 || null xs ||
  all ok chunks && concat chunks == xs
  where
    chunks = chunkBy n id xs

    ok [] = False
    ok [_] = True
    ok ys = sum ys <= n
