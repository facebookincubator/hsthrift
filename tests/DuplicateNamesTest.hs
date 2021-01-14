-- Copyright (c) Facebook, Inc. and its affiliates.

module DuplicateNamesTest where

import TestRunner
import Test.HUnit

import Duplicate.Types

x :: X
x = X
  { name = "XXX"
  , payload = 999
  }

y :: Y
y = Y
  { name = "XXX"
  , payload = True
  }

main :: IO ()
main = testRunner $ TestLabel "Duplicate Name Test" $ TestCase $
  assertEqual "same name" xname yname
  where
    xname = let X{..} = x in name
    yname = let Y{..} = y in name
