{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module TinyMTTest (main) where

import Test.HUnit
import TestRunner
import Util.HUnit (assertAbsError)

import Control.Monad (forM_)
import System.Random.TinyMT32
import System.Random.TinyMT64

generate :: (s -> (a, s)) -> s -> [a]
generate !f !s = let (r, t) = f s in r: generate f t

tinyMT32Test :: Test
tinyMT32Test = TestLabel "tinyMT32" $ TestCase $
  assertEqual "seed=1" (take 10 $ generate nextWord32 $ mkTinyMT32 1)
    -- https://github.com/MersenneTwister-Lab/TinyMT/blob/master/tinymt/check32.out.txt
    [ 2545341989,  981918433, 3715302833, 2387538352, 3591001365
    , 3820442102, 2114400566, 2196103051, 2783359912,  764534509
    ]

tinyMT64Test :: Test
tinyMT64Test = TestLabel "tinyMT64" $ TestCase $
  assertEqual "seed=1" (take 9 $ generate nextWord64 $ mkTinyMT64 1)
    -- https://github.com/MersenneTwister-Lab/TinyMT/blob/master/tinymt/check64.out.txt
    [ 15503804787016557143, 17280942441431881838,  2177846447079362065
    , 10087979609567186558,  8925138365609588954, 13030236470185662861
    ,  4821755207395923002, 11414418928600017220, 18168456707151075513
    ]

generateInts :: (Int, Int) -> [Int]
generateInts range = take 5 $ generate (nextIntR range) $ mkTinyMT64 5489

testNextRepeat :: Test
testNextRepeat = TestLabel "nextRepeat" $ TestCase $ do
  check "zero" 0
  check "negative" (-3)
  check "positive" 9
  check "minBound" minBound
  check "maxBound" maxBound
  where
    check msg value = assertEqual msg (replicate 5 value) $
      generateInts (value, value)

testNextIntR :: Test
testNextIntR = TestLabel "nextIntR" $ TestCase $ do
  check "positive" (2, 4) [2, 2, 4, 2, 4]
  check "negative" (-8, -4) [-4, -8, -7, -8, -6]
  check "small" (-2, 4) [-1, 2, 0, -1, 2]
  check "medium" (minBound `quot` 8 * 3, maxBound `quot` 8 * 3)
    [ 2395629012352244629, -1958463402975110527, -1645950220884638315
    , -1825383705551416783, -525145281743313058
    ]
  check "large" (minBound `quot` 4 * 3, maxBound `quot` 4 * 3)
    [ 4232731092176854464, 509404456177007115, -5674821519895684205
    , 3348059239475922435, 1365153161761685461
    ]
  check "nonpositive" (minBound, 0)
    [ -5674821519895684205, -8595595813991241892, -5747275563301060412
    , -5289501534423256805, -1386463226787267651
    ]
  check "nonnegative" (0, maxBound)
    [ 4232731092176854464, 509404456177007115, 8417830138486512255
    , 8730343320576984467, 8550909835910205999
    ]
  check "any" (minBound, maxBound)
    [ 4232731092176854464, 509404456177007115, -5674821519895684205
    , 8417830138486512255, 8730343320576984467
    ]
  where
    check msg range expected = assertEqual msg expected $ generateInts range

testNextDouble :: Test
testNextDouble = TestLabel "nextDouble" $ TestCase $
  forM_ (zip expected actual) $ \(e, a) ->
    assertAbsError "nextDouble" e 1e-8 a
  where
    actual = generate nextDouble $ mkTinyMT64 maxBound
    expected =
      [ 0.69015135575736
      , 0.20918409402853
      , 0.76313220670330
      , 0.70389801153176
      , 0.09553216832007
      ]

main :: IO ()
main = testRunner $ TestList
  [ tinyMT32Test
  , tinyMT64Test
  , testNextRepeat
  , testNextIntR
  , testNextDouble
  ]
