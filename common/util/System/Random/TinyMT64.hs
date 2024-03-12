{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | http://www.math.sci.hiroshima-u.ac.jp/m-mat/MT/TINYMT/
{-# LANGUAGE NumericUnderscores #-}
module System.Random.TinyMT64
  ( TinyMT64
  , mkTinyMT64
  , nextWord64
  , nextIntR
  , nextDouble
  ) where

import Control.Arrow (first)
import Data.Bits
import Data.Word

mat1, mat2, tmat, mask :: Word64
mat1 = 0xfa051f40
mat2 = 0xffd0fff4
tmat = 0x58d02ffeffbfffbc
mask = 0x7fffffffffffffff

data TinyMT64 = TinyMT64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64

mkTinyMT64 :: Word64 -> TinyMT64
mkTinyMT64 seed =
  rollState 7 $ rollState 6 $ rollState 5 $ rollState 4 $
  rollState 3 $ rollState 2 $ rollState 1 $
  TinyMT64 (mat2 `xor` tmat) (seed `xor` unsafeShiftL mat1 32)

rollState :: Word64 -> TinyMT64 -> TinyMT64
rollState !i (TinyMT64 s0 s1) =
  TinyMT64 s1 $ s0 `xor` b
  where
    a = s1 `xor` unsafeShiftR s1 62
    b = i + 6364136223846793005 * a

nextState :: TinyMT64 -> TinyMT64
nextState (TinyMT64 s0 s1) =
  TinyMT64 (s1 `xor` a) (x `xor` b)
  where
    x = x4
      where
        x0 = (s0 .&. mask) `xor` s1
        x1 = x0 `xor` unsafeShiftL x0 12
        x2 = x1 `xor` unsafeShiftR x1 32
        x3 = x2 `xor` unsafeShiftL x2 32
        x4 = x3 `xor` unsafeShiftL x3 11
    z = negate (x .&. 1)
    a = z .&. mat1
    b = z .&. unsafeShiftL mat2 32

temper :: TinyMT64 -> Word64
temper (TinyMT64 s0 s1) = t1 `xor` (negate (t1 .&. 1) .&. tmat)
  where
    t0 = s0 + s1
    t1 = t0 `xor` unsafeShiftR s0 8

nextWord64 :: TinyMT64 -> (Word64, TinyMT64)
nextWord64 !s = let t = nextState s in (temper t, t)

-- [min, max]
nextIntR :: Int -> Int -> TinyMT64 -> (Int, TinyMT64)
nextIntR !lo !hi
  | lo > hi = error "min>max"
  -- `range == 0` iff `lo == minBound && hi == maxBound`
  | range == 0 = nextFull
  -- Use different implementation for large range to make Hack implementation
  -- easier, where there is no good support for unsinged integers.
  | range >= 0x8000_0000_0000_0000 = nextLarge 20
  | otherwise = nextSmall 20
  where
    range = fromIntegral hi - fromIntegral lo + 1 :: Word64
    extra = (maxBound `rem` range + 1) `rem` range
    nextFull = first fromIntegral <$> nextWord64
    nextLarge !i !s
      | lo <= r && r <= hi = (r, t)
      | i > (1 :: Int) = nextLarge (i - 1) t
      | r < lo = (r + fromIntegral range, t)
      | otherwise {- r > hi -} = (r - fromIntegral range, t)
      where
        (r, t) = nextFull s
    nextSmall !i !s
      | r < extra && i > (1 :: Int) = nextSmall (i - 1) t
      | otherwise = (lo + fromIntegral (r `rem` range), t)
      where
        (r, t) = nextWord64 s

-- [0, 1]
nextDouble :: TinyMT64 -> (Double, TinyMT64)
nextDouble !s = (fromIntegral x / 1.8446744073709551616e19, t)
  where
    (x, t) = nextWord64 s
