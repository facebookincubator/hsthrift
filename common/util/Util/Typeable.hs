{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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
