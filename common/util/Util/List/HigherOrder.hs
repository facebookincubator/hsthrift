-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE PolyKinds #-}
module Util.List.HigherOrder
  ( fold2
  , map2
  , mapMaybe2
  , concatMap2
  , traverse2, traverse2_
  ) where

import Data.Foldable

-- | Efficiently right fold over two lists in sequence that have different
-- parameter types
fold2 :: (forall (z :: k). t z -> b -> b) -> b -> [t x] -> [t y] -> b
fold2 f b xs ys = foldr f (foldr f b ys) xs

-- | Efficiently map a normalizing function over two lists with different type
-- parameters
map2 :: (forall (z :: k). t z -> b) -> [t x] -> [t y] -> [b]
map2 f xs ys = foldr ((:) . f) (map f ys) xs

-- | Like map2, but filter out Nothings
mapMaybe2 :: (forall (z :: k). t z -> Maybe b) -> [t x] -> [t y] -> [b]
mapMaybe2 f = fold2 (maybe id (:) . f) []

concatMap2 :: (forall (z :: k). t z -> [b]) -> [t x] -> [t y] -> [b]
concatMap2 f xs ys = concat $ map2 f xs ys

traverse2
  :: Applicative f
  => (forall (z :: k). t z -> f a)
  -> [t x]
  -> [t y]
  -> f [a]
traverse2 f (x:xs) ys = (:) <$> f x <*> traverse2 f xs ys
traverse2 f [] ys = traverse f ys

traverse2_
  :: Applicative f
  => (forall (z :: k). t z -> f ())
  -> [t x]
  -> [t y]
  -> f ()
traverse2_ f (x:xs) ys = f x *> traverse2_ f xs ys
traverse2_ f [] ys = traverse_ f ys
