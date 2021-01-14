-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
module Thrift.Protocol.Binary.Internal
  ( binaryFloat, binaryDouble, getBuffer
  ) where

#if __GLASGOW_HASKELL__ == 804
import Thrift.Binary.Parser (getWord32be)
#endif
import Thrift.Binary.Parser as P
import Data.ByteString (ByteString)
import Data.Int
import Foreign.Marshal.Alloc
import Foreign.Storable as F
import Foreign.Ptr
import System.IO.Unsafe

{-# INLINE toFloat #-}
toFloat
  :: (Floating f, Storable f, Storable a)
  => a
  -> f
toFloat a =
  unsafeDupablePerformIO $ alloca $ \ptr ->
    poke ptr a >> F.peek (castPtr ptr)

{-# INLINE binaryFloat #-}
binaryFloat :: Parser Float
binaryFloat = toFloat <$> getWord32be

{-# INLINE binaryDouble #-}
binaryDouble :: Parser Double
binaryDouble = toFloat <$> getWord64be

{-# INLINE getBuffer #-}
-- | Parse a ByteString given a length parser and a copy function.
-- Note: The copy function *must* copy the input, otherwise this is unsafe
-- because the result is a slice of the input which may be a memory buffer that
-- is owned by C++. We also want to eliminate references to the input so that it
-- can get GC'd
getBuffer :: Parser Int32 -> (ByteString -> a) -> Parser a
getBuffer getLength copy = do
  len <- getLength
  buf <- P.getByteString $ fromIntegral len
  return $! copy buf
