-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.HUnit
  ( absError
  , assertAbsError
  , assertElem
  , assertSubsetOf
  , assertNotElem
  , assertPermutationOf
  , assertThrow
  ) where

import Control.Exception
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.List
import Test.HUnit


-- | The absolute error between an expected and actual value.
absError :: (Fractional a) => a -> a -> a
absError expected actual = abs (expected - actual)

-- | Asserts that the absolute error of a value is within a given
-- range. If you want the range to be in the realm of machine epsilon,
-- use @Data.AEq@ instead.
assertAbsError
  :: (Fractional a, Ord a, Show a)
  => String
  -- ^ Error message.
  -> a
  -- ^ Expected value.
  -> a
  -- ^ Error range (e.g., @1e-3@).
  -> a
  -- ^ Actual value.
  -> Assertion
assertAbsError message expected epsilon actual
  = flip assertBool (absError expected actual <= epsilon) $ concat
  [ message
  , " (expected "
  , show expected
  , "+/-"
  , show epsilon
  , " but got "
  , show actual
  , ")"
  ]

assertPermutationOf
  :: (Ord a, Show a)
  => String -> [a] -> [a] -> Assertion
assertPermutationOf m as bs = assertEqual m (sort as) (sort bs)

assertElem :: (Eq a, Show a, Foldable t) => String -> a -> t a -> Assertion
assertElem message expected actual =
  flip assertBool (expected `elem` actual) $ concat
    [ message
    , " (expected "
    , show expected
    , " to occur in "
    , show $ toList actual
    , ")"
    ]

assertSubsetOf
  :: (Eq a, Show a, Foldable t, Foldable f)
  => String
  -> f a
  -> t a
  -> Assertion
assertSubsetOf message expected actual =
  flip assertBool (all (`elem` actual) expected) $ concat
    [ message
    , " (expected all of"
    , show $ toList expected
    , " to occur in "
    , show $ toList actual
    , ")"
    ]

assertNotElem :: (Eq a, Show a, Foldable t) => String -> a -> t a -> Assertion
assertNotElem message expected actual =
  flip assertBool (not $ expected `elem` actual) $ concat
    [ message
    , " (expected "
    , show expected
    , " to not occur in "
    , show $ toList actual
    , ")"
    ]

assertThrow :: (Show a) => String -> IO a -> Assertion
assertThrow message test = do
  (actual :: Either SomeException a) <- try test
  flip assertBool (isLeft actual) $ concat
    [ message
    , " (expected: exception thrown; but got: "
    , show actual
    , ")"
    ]
