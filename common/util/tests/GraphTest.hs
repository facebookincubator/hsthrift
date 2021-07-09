-- Copyright (c) Facebook, Inc. and its affiliates.

module GraphTest (main) where

import Control.Monad
import qualified Data.IntMap as IntMap
import Data.Maybe

import Test.QuickCheck
import Test.HUnit
import TestRunner
import Util.Graph

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "postorder" $ TestCase $ do
      result <- quickCheckResult prop_postorder
      case result of
        Success{} -> return ()
        _ -> assertFailure "failed"
  ]

prop_postorder :: Property
prop_postorder = do
  forAll graphs $ \(nodes, outMap) ->
    check nodes outMap && check (reverse nodes) outMap
    -- regardless of the input order of the nodes, dependencies should
    -- appear before dependents in the output.
  where
  check nodes outMap =
    and [ posOf o < posOf n | n <- nodes, o <- out n ]
    where
      out n = fromJust (IntMap.lookup n outMap)
      order = postorder nodes id out
      pos = IntMap.fromList (zip order [(0::Int)..])
      posOf n = fromJust (IntMap.lookup n pos)

  graphs = do
    NonNegative numNodes <- arbitrary
    edges <- forM [1..numNodes] $ \n -> do
      outs <- sublistOf [1..n-1] -- only earlier nodes, so we get no cycles
      return (n,outs)
    return ([1..numNodes], IntMap.fromList edges)
