-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Typeable
  ( cast1
  ) where

import Data.Maybe
import Data.Typeable
import Unsafe.Coerce

-- | A simpler variant of Data.Typeable.gcast1
{-# INLINE cast1 #-}
cast1 :: (Typeable t, Typeable t') => t a -> Maybe (t' a)
cast1 x = r
  where
    r = if typeOf1 x == typeOf1 (fromJust r)
      then Just $ unsafeCoerce x
      else Nothing
