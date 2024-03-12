{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | http://www.math.sci.hiroshima-u.ac.jp/m-mat/MT/TINYMT/
module System.Random.TinyMT32
  ( TinyMT32
  , mkTinyMT32
  , nextWord32
  , generateWord32s
  ) where

import Data.Bits
import Data.Word

mat1, mat2, tmat, mask :: Word32
mat1 = 0x8f7011ee
mat2 = 0xfc78ff1f
tmat = 0x3793fdff
mask = 0x7fffffff

data TinyMT32 = TinyMT32
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32

mkTinyMT32 :: Word32 -> TinyMT32
mkTinyMT32 seed =
  nextState $ nextState $ nextState $ nextState $
  nextState $ nextState $ nextState $ nextState $
  rollState 7 $ rollState 6 $ rollState 5 $ rollState 4 $
  rollState 3 $ rollState 2 $ rollState 1 $
  TinyMT32 mat1 mat2 tmat $ fromIntegral seed

rollState :: Word32 -> TinyMT32 -> TinyMT32
rollState !i (TinyMT32 s0 s1 s2 s3) =
  TinyMT32 s1 s2 s3 $ s0 `xor` b
  where
    a = s3 `xor` unsafeShiftR s3 30
    b = i + 1812433253 * a

nextState :: TinyMT32 -> TinyMT32
nextState (TinyMT32 s0 s1 s2 s3) =
  TinyMT32 s1 (s2 `xor` a) (x `xor` unsafeShiftL y 10 `xor` b) y
  where
    x = x0 `xor` unsafeShiftL x0 1
      where
        x0 = (s0 .&. mask) `xor` s1 `xor` s2
    y = x `xor` s3 `xor` unsafeShiftR s3 1
    z = negate (y .&. 1)
    a = z .&. mat1
    b = z .&. mat2

temper :: TinyMT32 -> Word32
temper (TinyMT32 s0 _ s2 s3) = t0 `xor` t1 `xor` (negate (t1 .&. 1) .&. tmat)
  where
    t0 = s3
    t1 = s0 + unsafeShiftR s2 8

nextWord32 :: TinyMT32 -> (Word32, TinyMT32)
nextWord32 !s = let t = nextState s in (temper t, t)

generateWord32s :: TinyMT32 -> [Word32]
generateWord32s !s = let (x, t) = nextWord32 s in x: generateWord32s t
