-- Copyright (c) Facebook, Inc. and its affiliates.

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE CPP #-}

module Util.ByteString
  ( newCStringFromLazyByteString
  , useLazyByteStringAsCString
  , intToByteString
  , useAsCString
  , useAsCStringLen
  , useByteStringsAsCStrings
  , bsListAsCStrLenArr
  , unsafeBsListAsCStrLenArr
  ) where

import Control.Arrow (left, second)
import Control.Monad
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString as BS
import Foreign
import Foreign.C

import Data.ByteString.Builder
import Data.ByteString.Builder.Extra
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.Text as Text

import qualified Util.ASan as ASan

-- | Uses a lazy 'ByteString' as a NUL-terminated 'CString'.
useLazyByteStringAsCString
  :: Lazy.ByteString -> (CString -> IO a) -> IO a
useLazyByteStringAsCString = allocateAndCopy allocaBytes

-- | Copies a lazy 'ByteString' and adds a terminating NUL. The
-- resulting 'CString' must be 'free'd.
newCStringFromLazyByteString :: Lazy.ByteString -> IO CString
newCStringFromLazyByteString = flip (allocateAndCopy mallocBytes') return

useAsCStringLen :: BS.ByteString -> (CStringLen -> IO a) -> IO a
-- TODO(T24195918): We need to do this for ABI compatibility between
-- sigma.service and sigma.service.asan. Codemod to __SANITIZE_ADDRESS__
-- to enable ASAN checks for Haskell allocations.
#ifdef __HASKELL_SANITIZE_ADDRESS__
useAsCStringLen = ASan.byteStringWithCStringLen
#else
useAsCStringLen = BS.useAsCStringLen
#endif

useAsCString :: BS.ByteString -> (CString -> IO a) -> IO a
#ifdef __HASKELL_SANITIZE_ADDRESS__
useAsCString = ASan.byteStringWithCString
#else
useAsCString = BS.useAsCString
#endif

-- | Allocates space for a lazy 'ByteString' and NUL terminator, then
-- copies all its chunks into the buffer.
allocateAndCopy
  :: (Int -> (CString -> IO b) -> IO b)
  -> Lazy.ByteString
  -> (CString -> IO b)
  -> IO b
allocateAndCopy allocate lbs action = allocate (len + 1) $ \dst -> do
  copyChunksTo dst lbs
  pokeByteOff dst len (0::CChar)
  action dst

  where
  len :: Int
  len = fromIntegral (Lazy.length lbs)

  copyChunksTo :: Ptr a -> Lazy.ByteString -> IO ()
  copyChunksTo dst = foldM_ copyChunkTo 0 . Lazy.toChunks
    where
    copyChunkTo :: Int -> Strict.ByteString -> IO Int
    copyChunkTo off bs = BS.unsafeUseAsCStringLen bs $ \(src, chunkLen) -> do
      copyBytes (dst `plusPtr` off) src chunkLen
      return (off + chunkLen)

mallocBytes' :: Int -> (Ptr a -> IO b) -> IO b
mallocBytes' size action = mallocBytes size >>= action

lengthMinInt :: Int
lengthMinInt = length . show $ (minBound :: Int)

-- | Cheap (fast, low memory) conversion of Ints into an
-- ASCII/UTF8-encoded lazy ByteString.
intToByteString :: Int -> Lazy.ByteString
intToByteString =
  toLazyByteStringWith (untrimmedStrategy lengthMinInt 0) Lazy.empty . intDec

useByteStringsAsCStrings :: [BS.ByteString] -> (Ptr CString -> IO a) -> IO a
useByteStringsAsCStrings bs f = go bs []
  where go (t:ts) acc = useAsCString t $ go ts . (:acc)
        go []     acc = withArray0 nullPtr (reverse acc) f

bsListAsCStrLenArr :: [BS.ByteString]
                   -> (Ptr CString -> Ptr CSize -> CSize -> IO a)
                   -> IO a
bsListAsCStrLenArr bs f = go bs []
  where go (t:ts) acc = useAsCStringLen t $ go ts . (:acc)
        go []     acc = withArrayLen strs $ \len strPtr ->
                        withArray lens $ \lenPtr ->
                            f strPtr lenPtr (fromIntegral len)
            where (strs, lens) = second (map fromIntegral) $ unzip $ reverse acc

unsafeBsListAsCStrLenArr :: [BS.ByteString]
                   -> (Ptr CString -> Ptr CSize -> CSize -> IO a)
                   -> IO a
unsafeBsListAsCStrLenArr bs f = go bs []
  where go (t:ts) acc = BS.unsafeUseAsCStringLen t $ go ts . (:acc)
        go []     acc = withArrayLen strs $ \len strPtr ->
                        withArray lens $ \lenPtr ->
                            f strPtr lenPtr (fromIntegral len)
            where (strs, lens) = second (map fromIntegral) $ unzip $ reverse acc
