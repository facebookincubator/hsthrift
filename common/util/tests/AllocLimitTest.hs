-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE MagicHash #-}
module AllocLimitTest (main) where

import Test.HUnit
import TestRunner

import Util.AllocLimit

import Control.Exception
import GHC.Conc
import GHC.Exts

-- Test that GHC throws an alloc limit exception on code that *only*
-- allocates stack and not heap.
allocLimitStackTest :: Test
allocLimitStackTest = TestLabel "allocLimitStackTest" $ TestCase $ do
  let f :: Int# -> Int#
      f x = if isTrue# (x <# 0#)        -- avoid optimising away x
               then 1#
               else f (x +# 1#) -# 1#  -- should just grow the stack

  setAllocationCounter (10*1024*1024)
  enableAllocationLimit
  r <- try $ evaluate (isTrue# (f 0# ==# 0#))
  disableAllocationLimit
  assertBool "allocLimitTest" $ case r of
    Left e | Just AllocationLimitExceeded <- fromException e -> True
    _ -> False

limitAllocsTest :: Test
limitAllocsTest = TestLabel "limitAllocs" $ TestCase $ do
  e <- limitAllocs 10000 $ evaluate $ sum [(1::Integer)..]
  assertEqual "limitAllocs Nothing" Nothing e

main :: IO ()
main = testRunner $ TestList
  [ allocLimitStackTest
  , limitAllocsTest
  ]
