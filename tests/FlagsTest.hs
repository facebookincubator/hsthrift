-- Copyright (c) Facebook, Inc. and its affiliates.

module FlagsTest where

import Test.HUnit
import TestRunner
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Flags.Types

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "intConst" $ intConst ~?= (100 :: Int)
  , TestLabel "mapConst" $
    mapConst ~?= HashMap.fromList [("a", 1), ("b", 2)]
  , TestLabel "setConst" $ setConst ~?= HashSet.fromList [1, 2, 3]
  ]
