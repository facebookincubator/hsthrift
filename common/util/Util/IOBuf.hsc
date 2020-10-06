module Util.IOBuf
  ( unsafeWithIOBuf
  , IOBuf
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString.Lazy as Lazy

#include <common/hs/cpp/IOBuf.h>

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
