-- Copyright (c) Facebook, Inc. and its affiliates.

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE CPP #-}

module Util.Memory
  ( alloca
  , allocaBytes
  , allocaBytesAligned
  , allocaArray
  , allocaArray0
  , with
  ) where

import qualified Util.ASan as A
import qualified Foreign as F

{-# INLINE alloca #-}
alloca :: F.Storable a => (F.Ptr a -> IO b) -> IO b
-- TODO(T24195918): We need to do this for ABI compatibility between
-- sigma.service and sigma.service.asan. Codemod to __SANITIZE_ADDRESS__
-- to enable ASAN checks for Haskell allocations.
#ifdef __HASKELL_SANITIZE_ADDRESS__
alloca = A.alloca
#else
alloca = F.alloca
#endif

{-# INLINE allocaBytes #-}
allocaBytes :: Int -> (F.Ptr a -> IO b) -> IO b
#ifdef __HASKELL_SANITIZE_ADDRESS__
allocaBytes = A.allocaBytes
#else
allocaBytes = F.allocaBytes
#endif

{-# INLINE allocaBytesAligned #-}
allocaBytesAligned :: Int -> Int -> (F.Ptr a -> IO b) -> IO b
#ifdef __HASKELL_SANITIZE_ADDRESS__
allocaBytesAligned = A.allocaBytesAligned
#else
allocaBytesAligned = F.allocaBytesAligned
#endif

{-# INLINE allocaArray #-}
allocaArray :: F.Storable a => Int -> (F.Ptr a -> IO b) -> IO b
#ifdef __HASKELL_SANITIZE_ADDRESS__
allocaArray = A.allocaArray
#else
allocaArray = F.allocaArray
#endif

{-# INLINE allocaArray0 #-}
allocaArray0 :: F.Storable a => Int -> (F.Ptr a -> IO b) -> IO b
#ifdef __HASKELL_SANITIZE_ADDRESS__
allocaArray0 = A.allocaArray0
#else
allocaArray0 = F.allocaArray0
#endif

{-# INLINE with #-}
with :: F.Storable a => a -> (F.Ptr a -> IO b) -> IO b
#ifdef __HASKELL_SANITIZE_ADDRESS__
with = A.with
#else
with = F.with
#endif
