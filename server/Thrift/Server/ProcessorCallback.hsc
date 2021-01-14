-- Copyright (c) Facebook, Inc. and its affiliates.

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
import Data.Typeable (typeOf)
import Foreign
import Foreign.C
import Thrift.Monad
import Thrift.Processor
import Thrift.Protocol.Id

#include <cpp/HaskellProcessor.h>

withProcessorCallback :: (Processor s)
                      => (forall r . s r -> IO r) -- ^ handler to use
                      -> (FunPtr ProcessorCallback -> IO a)
                      -> IO a
withProcessorCallback handler =
  bracket (makeProcessorCallback handler) deleteProcessorCallback

makeProcessorCallback :: (Processor s)
                      => (forall r . s r -> IO r) -- ^ handler to use
                      -> IO (FunPtr ProcessorCallback)
makeProcessorCallback handler = do
  counter <- newCounter
  mkProcessorCallback $ handlerWrapper counter handler

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
               -> ProcessorCallback
handlerWrapper counter handler prot_id input_str input_len response_ptr = do
  seqNum <- counter
  input <- BS.unsafePackCStringLen (input_str, fromIntegral input_len)
  (res, exc) <- withProxy (fromIntegral prot_id) $ \proxy ->
    process proxy seqNum handler input
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
