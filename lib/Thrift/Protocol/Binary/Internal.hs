{-# LANGUAGE CPP #-}
--
-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements. See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership. The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- License); you may not use this file except in compliance
-- with the License. You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing,
-- software distributed under the License is distributed on an
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied. See the License for the
-- specific language governing permissions and limitations
-- under the License.
--

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
