-- Copyright (c) Facebook, Inc. and its affiliates.

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
        (sort "Object (HashMap.fromList [(\"foo\", Number (-3)), (\"bar\", Bool True)])")
        (sort $ pp $ object
          [ "foo" .= Number (-3)
          , "bar" .= Bool True
          ])
  ]

main :: IO ()
main = testRunner tests
