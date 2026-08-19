{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ForeignFunctionInterface #-}

-- | The inbound Thrift @client_id@ of the request being handled.
--
-- hsthrift does not surface inbound transport headers to Haskell handlers.
-- 'HaskellAsyncProcessor' publishes this one field for the duration of its
-- synchronous callback into Haskell (see @cpp/HaskellProcessor.cpp@), and this
-- module reads it back.
module Thrift.Server.ClientId
  ( getInboundClientId
  ) where

import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

foreign import ccall unsafe "c_hsthrift_inbound_client_id"
  c_hsthrift_inbound_client_id :: Ptr CSize -> IO CString

-- | The @client_id@ the caller's transport sent with the request currently
-- being handled, or the empty string when it sent none.
--
-- Must be called from the handler, on the thread the request was dispatched
-- to: the value is only published for the duration of that call. Read it
-- before handing work to another thread.
--
-- The import is @unsafe@ so it runs inline on that thread rather than letting
-- the RTS move the call elsewhere.
getInboundClientId :: IO Text
getInboundClientId = alloca $ \lenPtr -> do
  str <- c_hsthrift_inbound_client_id lenPtr
  if str == nullPtr
    then return Text.empty
    else do
      len <- peek lenPtr
      -- packCStringLen copies, which it must: the C string is owned by the
      -- request and dies with it, whereas decoding may be forced later.
      bytes <- ByteString.packCStringLen (str, fromIntegral len)
      return $! decodeUtf8With lenientDecode bytes
