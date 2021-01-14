-- Copyright (c) Facebook, Inc. and its affiliates.

module ConstGenTest where

import Test.HUnit
import TestRunner
import Data.Default (def)
import Data.Int (Int64)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as VectorStorable

import Constants.Types

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "i32Const" $ i32Const ~?= 99
  , TestLabel "boolConst" $ boolConst ~?= False
  , TestLabel "floatConst" $ floatConst ~?= 0.5
  , TestLabel "enumNum" $ enumNum ~?= X_B
  , TestLabel "enumConst" $ enumConst ~?= X_D
  , TestLabel "enumList" $ enumList ~?= [X_A, X_B, X_C, X_D]
  , TestLabel "mapConst" $
    mapConst ~?= Map.fromList [(0, "zero"), (1, "one")]
  , TestLabel "fooConst" $ fooConst ~?= Foo Nothing "hello world"
  , TestLabel "partial" $ partial ~?= Foo Nothing "X"
  , TestLabel "idConst" $ idConst ~?= Id 12345
  , TestLabel "idVect" $ idVect ~?= Vector.fromList (map Id [1..5])
  , TestLabel "i64VectS" $ i64VectS ~?= VectorStorable.fromList [1..5 :: Int64]
  , TestLabel "hashmapConst" $
    hashmapConst ~?= HashMap.fromList [(0, "zero"), (1, "one")]
  , TestLabel "strConst" $ strConst ~?= ("string" :: String)
  , TestLabel "NagativeFields.def" $ def ~?= NagativeFields
      { nagativeFields_u = -1
      , nagativeFields_v = Nothing
      , nagativeFields_w = -2
      }
  , TestLabel "NonEmpty.def" $ def ~?= NonEmpty_ne 0
  , TestLabel "Const True" $ trueConst ~?= True
  , TestLabel "Const False" $ falseConst ~?= False
  ]
