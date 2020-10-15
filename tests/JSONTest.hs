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

{-# OPTIONS_GHC -fno-warn-orphans #-}
module JSONTest where

import Control.Applicative
import System.Exit
import Test.QuickCheck
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Vector.Storable as VectorStorable
import Foreign.Storable (Storable)

import Foo.Types
import Thrift.Protocol.JSON

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = Vector.fromList <$> arbitrary

instance (Arbitrary a, Storable a) => Arbitrary (VectorStorable.Vector a) where
  arbitrary = VectorStorable.fromList <$> arbitrary

instance Arbitrary Bar where
  arbitrary = Bar <$> arbitrary <*> (Text.pack <$> arbitrary)

instance Arbitrary Foo where
  arbitrary =
    Foo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary

prop_roundTrip :: Foo -> Bool
prop_roundTrip = liftA2 (==) Right (deserializeJSON . serializeJSON)

main :: IO ()
main = do
  result <- quickCheckResult prop_roundTrip
  case result of
    Success{} -> exitSuccess
    _         -> exitFailure
