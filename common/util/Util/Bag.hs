-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Bag
  ( Bag -- Do not export constructors.
  , singleton
  , empty
  ) where

import Data.Foldable (toList)

-- | A bag of items which supports O(1) append
data Bag a
  = EmptyBag
  | OneItem a
  | ConcatBags (Bag a) (Bag a) -- INVARIANT: Neither sub-bag is empty

instance Semigroup (Bag a) where
  (<>) = mappend

instance Monoid (Bag a) where
  mempty = EmptyBag

  mappend EmptyBag y = y
  mappend x EmptyBag = x
  mappend x y = ConcatBags x y

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

instance Show a => Show (Bag a) where
  show b = "fromList " ++ show (toList b)
