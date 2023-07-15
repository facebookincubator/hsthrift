{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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
