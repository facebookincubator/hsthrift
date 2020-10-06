-- Copyright (c) 2014, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE MagicHash #-}
module Util.Bits (bitLength) where

import GHC.Exts
import GHC.Integer.Logarithms

-- | The number of bits in the minimal two's-complement
-- representation, excluding a sign bit.
--
-- > bitLength 3 = 2
-- > bitLength 4 = 3
--
-- See http://mathworld.wolfram.com/BitLength.html for more information.
bitLength :: Integral a => a -> Int
bitLength = integerBitLength . toInteger
  where
  integerBitLength n
    | n < 0 = integerBitLength $ -1 - n
    | n == 0 = 0
    | otherwise = I# (1# +# integerLog2# n)
