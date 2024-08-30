{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.Bag
  ( Bag -- Do not export constructors.
  , singleton
  , empty
  , fromList
  ) where

import Data.Foldable (toList)

-- | A bag of items which supports O(1) append
data Bag a
  = EmptyBag
  | OneItem a
  | ConcatBags (Bag a) (Bag a) -- INVARIANT: Neither sub-bag is empty

instance Semigroup (Bag a) where
  (<>) EmptyBag y = y
  (<>) x EmptyBag = x
  (<>) x y = ConcatBags x y

instance Monoid (Bag a) where
  mempty = EmptyBag

instance Foldable Bag where
  foldr _ b EmptyBag = b
  foldr f b (OneItem x) = f x b
  foldr f b (ConcatBags x y) = foldr f (foldr f b y) x

instance Functor Bag where
  fmap _ EmptyBag = EmptyBag
  fmap f (OneItem x) = OneItem $ f x
  fmap f (ConcatBags x y) = ConcatBags (fmap f x) (fmap f y)

singleton :: a -> Bag a
singleton x = OneItem x

empty :: Bag a -> Bool
empty EmptyBag = True
empty _ = False

fromList :: [a] -> Bag a
fromList = mconcat . map singleton

instance Show a => Show (Bag a) where
  show b = "fromList " ++ show (toList b)
