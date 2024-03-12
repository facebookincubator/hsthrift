{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module TinyMTTest (main) where

import Test.HUnit
import TestRunner

import System.Random.TinyMT32
import System.Random.TinyMT64

tinyMT32Test :: Test
tinyMT32Test = TestLabel "tinyMT32" $ TestCase $
  assertEqual "seed=1" (take 10 $ generateWord32s $ mkTinyMT32 1)
    -- https://github.com/MersenneTwister-Lab/TinyMT/blob/master/tinymt/check32.out.txt
    [ 2545341989,  981918433, 3715302833, 2387538352, 3591001365
    , 3820442102, 2114400566, 2196103051, 2783359912,  764534509
    ]

tinyMT64Test :: Test
tinyMT64Test = TestLabel "tinyMT64" $ TestCase $
  assertEqual "seed=1" (take 9 $ generateWord64s $ mkTinyMT64 1)
    -- https://github.com/MersenneTwister-Lab/TinyMT/blob/master/tinymt/check64.out.txt
    [ 15503804787016557143, 17280942441431881838,  2177846447079362065
    , 10087979609567186558,  8925138365609588954, 13030236470185662861
    ,  4821755207395923002, 11414418928600017220, 18168456707151075513
    ]

main :: IO ()
main = testRunner $ TestList
  [ tinyMT32Test
  , tinyMT64Test
  ]
