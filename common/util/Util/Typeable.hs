-- Copyright (c) 2014, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

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
