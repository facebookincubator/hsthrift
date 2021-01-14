-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module HasFieldTest where

import GHC.Int

import Thrift.HasFields

import TestRunner
import Test.HUnit

import qualified Data.Vector as Vector

import Hasfield.Types

foo :: Foo
foo = Foo
  { foo_foo1 = Just 1
  , foo_foo2 = True
  , foo_foo3 = bar
  , foo_foo4 = 420
  , foo_foo5 = Vector.fromList [420]
  , foo_foo_foo1 = 42
  }

bar :: Bar
bar = Bar
  { bar_bar1 = 13
  , bar_bar2 = "asdf"
  , bar_foo1 = Just 1 -- Shared field with foo
  , bar_foo2 = 38439 -- Not shared, different type than foo._foo2
  }

grabFoo1 :: HasField "foo1" a (Maybe Int64) => a -> Maybe Int64
grabFoo1 = getField @"foo1"

main :: IO ()
main = testRunner $ TestLabel "HasField Test" $ TestCase $ do
  let
    -- Assert the fields are different types
    x = getField @"foo2" bar :: Int64
    y = getField @"foo2" foo :: Bool
  assertEqual "grabbed int field is right" x (bar_foo2 bar)
  assertEqual "grabbed bool field is right" y (foo_foo2 foo)
  assertEqual "grabbed struct field is right"
    (getField @"foo3" foo :: Bar) bar
  assertEqual "grabbed vector field is right"
    (getField @"foo5" foo :: Vector.Vector Int64) (foo_foo5 foo)
  assertEqual "can get foo1 from both" (grabFoo1 foo) (grabFoo1 bar)
