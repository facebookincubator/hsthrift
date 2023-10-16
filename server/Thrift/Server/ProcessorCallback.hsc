{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Thrift.Server.ProcessorCallback
  ( withProcessorCallback
  , makeProcessorCallback
  , deleteProcessorCallback

  , ProcessorCallback
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Unsafe as BS
import Data.Foldable (for_)
import Data.Typeable (typeOf)
import Foreign
import Foreign.C
import Thrift.Monad
import Thrift.Processor
import Thrift.Protocol.Id

#include <cpp/HaskellProcessor.h>

withProcessorCallback :: (Processor s)
                      => (forall r . s r -> IO r) -- ^ handler to use
                      -> (forall r . s r -> Either SomeException r -> Header)
                      -> (FunPtr ProcessorCallback -> IO a)
                      -> IO a
withProcessorCallback handler postProcess =
  bracket (makeProcessorCallback handler postProcess) deleteProcessorCallback

makeProcessorCallback :: (Processor s)
                      => (forall r . s r -> IO r) -- ^ handler to use
                      -> (forall r . s r -> Either SomeException r -> Header)
                      -> IO (FunPtr ProcessorCallback)
makeProcessorCallback handler postProcess = do
  counter <- newCounter
  mkProcessorCallback $ handlerWrapper counter handler postProcess

deleteProcessorCallback :: FunPtr ProcessorCallback -> IO ()
deleteProcessorCallback = freeHaskellFunPtr

-- -----------------------------------------------------------------------------

data TResponse

-- | A function that will be called from C back into Haskell
-- Takes protocol id, input data, input length, pointer to fill with output
-- length, and returns a newly malloc'd set of bytes
type ProcessorCallback = CUShort -> CString -> CSize -> Ptr TResponse -> IO ()

-- Entry point for every request coming into Haskell.
handlerWrapper :: (Processor s)
               => Counter
               -> (forall r . s r -> IO r)
               -> (forall r . s r -> Either SomeException r -> Header)
               -> ProcessorCallback
handlerWrapper counter handler postProcess prot_id input_str input_len response_ptr = do
  seqNum <- counter
  input <- BS.unsafePackCStringLen (input_str, fromIntegral input_len)
  (res, exc, headers) <- withProxy (fromIntegral prot_id) $ \proxy ->
    process proxy seqNum handler postProcess input
  (output_str, output_len) <- newByteStringAsCStringLen res
  #{poke apache::thrift::TResponse, data} response_ptr output_str
  #{poke apache::thrift::TResponse, len} response_ptr
    (fromIntegral output_len :: CSize)
  case exc of
    Just (SomeException ex, blame) -> do
      (ex_name, ex_name_len) <- newByteStringAsCStringLen
        $ UTF8.fromString
        $ show
        $ typeOf ex
      (ex_text, ex_text_len) <- newByteStringAsCStringLen
        $ UTF8.fromString
        $ take 1024
        $ show ex
      #{poke apache::thrift::TResponse, ex_name} response_ptr ex_name
      #{poke apache::thrift::TResponse, ex_name_len} response_ptr
        (fromIntegral ex_name_len :: CSize)
      #{poke apache::thrift::TResponse, ex_text} response_ptr ex_text
      #{poke apache::thrift::TResponse, ex_text_len} response_ptr
        (fromIntegral ex_text_len :: CSize)
      #{poke apache::thrift::TResponse, client_error} response_ptr
        (blame == ClientError)
    Nothing -> do
      #{poke apache::thrift::TResponse, ex_name} response_ptr nullPtr
      #{poke apache::thrift::TResponse, ex_name_len} response_ptr (0 :: CSize)
      #{poke apache::thrift::TResponse, ex_text} response_ptr nullPtr
      #{poke apache::thrift::TResponse, ex_text_len} response_ptr (0 :: CSize)

  for_ headers $ \(n,v) ->
    BS.unsafeUseAsCStringLen n $ \(n_str, n_len) ->
    BS.unsafeUseAsCStringLen v $ \(v_str, v_len) ->
      addHeaderToResponse
        response_ptr n_str (fromIntegral n_len) v_str (fromIntegral v_len)

  where
    -- Allocates a new buffer to give away ownership of memory
    newByteStringAsCStringLen :: ByteString -> IO CStringLen
    newByteStringAsCStringLen bs =
      BS.unsafeUseAsCStringLen bs $ \(src, len) -> do
        buf <- mallocBytes len
        copyBytes buf src len
        return (buf, len)

foreign import ccall "wrapper"
  mkProcessorCallback :: ProcessorCallback -> IO (FunPtr ProcessorCallback)

foreign import ccall unsafe "addHeaderToResponse"
  addHeaderToResponse
    :: Ptr TResponse -> CString -> CSize -> CString -> CSize -> IO ()
