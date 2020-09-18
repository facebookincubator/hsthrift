--
-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements. See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership. The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- License); you may not use this file except in compliance
-- with the License. You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing,
-- software distributed under the License is distributed on an
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied. See the License for the
-- specific language governing permissions and limitations
-- under the License.
--

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
