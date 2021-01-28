-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.IOBuf
  ( unsafeWithIOBuf
  , IOBuf
  , toLazy
  ) where

import Control.Exception (mask_)
import Data.ByteString.Internal
import Data.ByteString.Unsafe
import Data.Word
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict

#include <cpp/IOBuf.h>

-- | This is the representation of a C++ HS_IOBuf. It has no representation in
-- Haskell therefore we use an uninhabited type
data IOBuf

-- We need a Storable instance in order to alloca an HS_IOBuf from haskell
instance Storable IOBuf where
  sizeOf _ = (#size HS_IOBuf)
  alignment = sizeOf
  -- IOBuf is an uninhabited type, so peek and poke are not implemented
  peek = error "peek: unimplemented"
  poke = error "poke: unimplemented"

-- | Marshal a lazy ByteString to a C++ HS_IOBuf and call a function on the
-- result. This is unsafe because it uses the underlying bytestring buffers of
-- the Haskell ByteString in C++. If this gets garbage collected while it is
-- still being used in C++, then the program will crash.
unsafeWithIOBuf :: Lazy.ByteString -> (Ptr IOBuf -> IO a) -> IO a
unsafeWithIOBuf bs f =
  withChain (Lazy.toChunks bs) $ \str_arr len_arr len ->
  alloca $ \hs_iobuf -> do
    (#poke HS_IOBuf, str_arr) hs_iobuf str_arr
    (#poke HS_IOBuf, len_arr) hs_iobuf len_arr
    (#poke HS_IOBuf, len) hs_iobuf len
    f hs_iobuf

withChain
  :: [ByteString]
  -> (Ptr CString -> Ptr CSize -> CSize -> IO a)
  -> IO a
withChain chunks f = withMany unsafeUseAsCStringLen chunks $ \cstrs ->
  let
    (strs, lens) = unzip cstrs
  in
    withArrayLen strs $ \len strings ->
    withArray (map fromIntegral lens) $ \lengths ->
    f strings lengths (fromIntegral len)

data CIOBufData

-- The ForeignPtr points to the data buffer, not the IOBuf, but
-- the finalizer calls the destructor on the IOBuf*.
-- This function takes ownership of Ptr IOBuf.
toByteString :: Ptr IOBuf -> Int -> Ptr Word8 -> IO Strict.ByteString
toByteString p p_len p_buf = mask_ $ do
  fp <- newForeignPtrEnv c_destroy p p_buf
  return $ fromForeignPtr fp 0 p_len

-- | A mock fmap over the IOBuf chain to create strict ByteStrings.
bufToChunks
  :: Ptr IOBuf
  -> IO [Strict.ByteString]
bufToChunks ptr
  | ptr == nullPtr = pure []
  | otherwise = allocaBytes #{size IOBufData} $ \p_data -> do
      c_get_data ptr p_data
      p_len <- #{peek IOBufData, length_} p_data
      p_buf <- #{peek IOBufData, data_buf_} p_data
      p_next <- #{peek IOBufData, next_} p_data
      (:) <$> toByteString ptr p_len p_buf
          <*> bufToChunks p_next

-- | Marshall a folly::IOBuf to a lazy ByteString. Could also fold and append
-- to a lazy ByteString instead of using fromChunks.
toLazy :: Ptr IOBuf -> IO Lazy.ByteString
toLazy p = Lazy.fromChunks <$>
  (mask_ $ bufToChunks p)

foreign import ccall unsafe "get_iobuf_data" c_get_data
  :: Ptr IOBuf -> Ptr CIOBufData -> IO ()
foreign import ccall unsafe "&destroy_iobuf" c_destroy
  :: FinalizerEnvPtr IOBuf Word8
