{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module AsyncTest (main) where

import Data.Monoid
import Test.HUnit
import TestRunner

import Util.Async

windowUnorderedReduceTest :: Test
windowUnorderedReduceTest = TestLabel "windowUnorderedReduce"
  $ TestCase $ do
    r0 <- windowUnorderedReduce 1 identity [1..10]
    assertEqual "test0" 55 $ sum r0
    r1 <- windowUnorderedReduce 3 identity [1..10]
    assertEqual "test1" 55 $ sum r1
    r2 <- windowUnorderedReduce 16 identity [1..10]
    assertEqual "test2" 55 $ sum r2
  where
    identity :: Int -> IO (Sum Int)
    identity a = return $ Sum a

main :: IO ()
main = testRunner $ TestList
  [ windowUnorderedReduceTest
    ]
