-- Copyright (c) Facebook, Inc. and its affiliates.

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
