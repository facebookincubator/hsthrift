{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module ToExpTest where

import Test.HUnit
import TestRunner

import Data.Aeson ((.=), Value(Array, Number, Bool), object)
import qualified Data.Aeson as A
import Data.List
import qualified Data.Vector as Vector
import Util.ToExp

tests :: Test
tests = TestList
  [ TestLabel "numbers" $ TestCase $ do
      assertEqual "int"
        "3"
        (pp (3 :: Int))
      assertEqual "negative int"
        "(-7)"
        (pp (-7 :: Int))
      assertEqual "negative double"
        "(-7.0)"
        (pp (-7.0 :: Double))
      assertEqual "negative number in "
        "Just (-7)"
        (pp (Just (-7) :: Maybe Int))
  , TestLabel "json" $ TestCase $ do
      assertEqual "array"
        "Array (Vector.fromList [Number (-3), Bool True, String \"foobarbaz\"])"
        (pp $ Array $ Vector.fromList
          [ Number (-3)
          , Bool True
          , A.String "foobarbaz"
          ])
      assertEqual "object"
        -- the result is non-deterministic. Rather than add the
        -- overhead of sorting the elements all the time, let's just
        -- sort the test output.
        (sort "Object (fromList [(fromText \"foo\", Number (-3)), (fromText \"bar\", Bool True)])")
        (sort $ pp $ object
          [ "foo" .= Number (-3)
          , "bar" .= Bool True
          ])
  ]

main :: IO ()
main = testRunner tests
