-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.ASan
  ( alloca
  , allocaBytes
  , allocaArray
  , allocaArray0
  , allocaBytesAligned
  , with
  , byteStringWithCString
  , byteStringWithCStringLen
  , textWithCStringLen
  , textWithCString
  , textUseAsPtr
  ) where

import Control.Exception

import Foreign hiding
  (alloca, allocaBytes, allocaBytesAligned, allocaArray, allocaArray0, with)
import Foreign.C

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI

import Data.Text.Internal (Text(..))
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Foreign as TF

-- The following functions already support ASan (they already allocate using
-- malloc):
-- newCStringFromText

alloca :: Storable a => (Ptr a -> IO b) -> IO b
alloca = doAlloca undefined
  where
    doAlloca :: Storable a' => a' -> (Ptr a' -> IO b') -> IO b'
    doAlloca dummy = allocaBytesAligned (sizeOf dummy) (alignment dummy)

allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
allocaBytes size = bracket (mallocBytes size) free

-- alignedAlloc calls an aligned allocation function that is platform-dependent
-- void *alignedAlloc( size_t alignment, size_t size );
foreign import ccall unsafe "alignedAlloc"
  cAlignedAlloc :: CSize -> CSize -> IO (Ptr a)

allocaBytesAligned :: Int -> Int -> (Ptr a -> IO b) -> IO b
allocaBytesAligned size alignment =
  bracket (cAlignedAlloc (fromIntegral alignment) (fromIntegral size)) free

allocaArray :: Storable a => Int -> (Ptr a -> IO b) -> IO b
allocaArray  = doAlloca undefined
  where
    doAlloca :: Storable a' => a' -> Int -> (Ptr a' -> IO b') -> IO b'
    doAlloca dummy size = allocaBytesAligned (size * sizeOf dummy)
                                              (alignment dummy)

allocaArray0      :: Storable a => Int -> (Ptr a -> IO b) -> IO b
allocaArray0 size  = allocaArray (size + 1)

with :: Storable a => a -> (Ptr a -> IO b) -> IO b
with val f =
  alloca $ \ptr -> do
    poke ptr val
    f ptr

-- Text / ByteString marshalling

textWithCString :: Text -> (CString -> IO a) -> IO a
textWithCString = byteStringWithCString . encodeUtf8

byteStringWithCString :: BS.ByteString -> (CString -> IO a) -> IO a
byteStringWithCString (BI.PS fp off len) fun =
  allocaBytes (len+1) $ \ptr' -> do
    withForeignPtr fp $ \ptr -> do
      copyBytes ptr' (ptr `plusPtr` off) len
      poke (ptr' `plusPtr` len) (0 :: Word8)
    fun ptr'


textWithCStringLen :: Text -> (CStringLen -> IO a) -> IO a
textWithCStringLen = byteStringWithCStringLen . encodeUtf8

byteStringWithCStringLen :: BS.ByteString -> (CStringLen -> IO a) -> IO a
byteStringWithCStringLen (BI.PS _ _ 0) fun = fun (nullPtr, 0)
byteStringWithCStringLen (BI.PS fp off len) fun =
  allocaBytes len $ \ptr' -> do
    withForeignPtr fp $ \ptr -> copyBytes ptr' (ptr `plusPtr` off) len
    fun (ptr', len)

-- Text marshalling

textUseAsPtr :: Text -> (Ptr Word16 -> TF.I16 -> IO a) -> IO a
textUseAsPtr t@(Text _arr _off len) action =
    allocaBytes (len * 2) $ \buf -> do
      TF.unsafeCopyToPtr t buf
      action (castPtr buf) (fromIntegral len)
