-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fprof-auto #-}
module Thrift.Channel.CppChannel
  ( WrappedChannel(..), CppRequestChannelPtr
  , withCppChannelIO, withCppChannel, getInnerCppRequestChannel
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Unsafe
##if __GLASGOW_HASKELL__ < 804
import Data.Monoid
##endif
import Data.Text.Encoding
import Foreign hiding (void)
import Foreign.C
import GHC.Conc (newStablePtrPrimMVar, PrimMVar)
import TextShow

import Thrift.Channel
import Thrift.Monad
import Thrift.Protocol.Binary
import Util.Control.Exception
import Util.Log

-- | Encapsulation for using the C++ thrift libraries to make requests
--
-- NOTE, resource lifetime management is Hard with C++:
-- * These must be used only within the scope of `withFacebook` and
--   `withEventBaseDataplane`.
-- * Do not use the global IOExecutor with Haskell Thrift channels. This way
--   lies madness.


-- Things that exist in C++
data CppWrappedChannel
data CppRequestChannelPtr

#include <cpp/HsChannel.h>

-- | WrappedChannel is parameterized by a phantom type that represents the
-- specific CPP client channel we are using
newtype WrappedChannel t s = WrappedChannel
  { cppChannel :: Ptr CppWrappedChannel
  }

withCppChannel
  :: Ptr CppRequestChannelPtr
  -> ThriftM p (WrappedChannel t) s a
  -> IO a
withCppChannel channel = withCppChannelIO channel . runThrift

withCppChannelIO
  :: Ptr CppRequestChannelPtr
  -> (WrappedChannel t s -> IO a)
  -> IO a
withCppChannelIO channel action =
  bracket (c_newWrapper channel) c_deleteWrapper $ \ch ->
    action (WrappedChannel ch)

-- | Returns a raw pointer to the inner channel.
-- This is only valid while the wrapped channel is alive.
getInnerCppRequestChannel
  :: WrappedChannel t s
  -> IO (Ptr CppRequestChannelPtr)
getInnerCppRequestChannel WrappedChannel{..} =
  c_getInnerRequestChannel cppChannel

--------------------------------------------------------------------------------

instance ClientChannel (WrappedChannel t) where
  sendRequest WrappedChannel{..} Request{..} sendCob recvCob = mask_ $ do
    (send_mvar, send_sp, send_result) <- newCallbackMVar
    (recv_mvar, recv_sp, recv_result) <- newCallbackMVar
    (cap,_) <- threadCapability =<< myThreadId
    forkIO $ do
      cont <- sendCollector send_mvar send_result sendCob reqMsg
      when cont $ recvCollector recv_mvar recv_result recvCob
    withForeignPtr send_result $ \send_result_p -> do
    withForeignPtr recv_result $ \recv_result_p -> do
    unsafeUseAsCStringLen reqMsg $ \(buf, len) -> do
    unsafeUseAsCStringLen (serializeBinary reqOptions) $ \(oBuf, oLen) ->
      c_sendReq
        cppChannel
        buf
        (fromIntegral len)
        (fromIntegral cap)
        send_sp
        recv_sp
        send_result_p
        recv_result_p
        oBuf
        (fromIntegral oLen)

  sendOnewayRequest WrappedChannel{..} Request{..} sendCob = mask_ $ do
    (send_mvar, send_sp, send_result) <- newCallbackMVar
    forkIO $ void $ sendCollector send_mvar send_result sendCob reqMsg
    (cap,_) <- threadCapability =<< myThreadId
    withForeignPtr send_result $ \send_result_p -> do
    unsafeUseAsCStringLen reqMsg $ \(buf, len) -> do
    unsafeUseAsCStringLen (serializeBinary reqOptions) $ \(oBuf, oLen) ->
      c_sendOnewayReq
        cppChannel
        buf
        (fromIntegral len)
        (fromIntegral cap)
        send_sp
        send_result_p
        oBuf
        (fromIntegral oLen)

sendCollector
  :: MVar () -> ForeignPtr CFinishedRequest -> SendCallback -> ByteString
  -> IO Bool
sendCollector send_mvar send_result sendCob reqMsg = do
  takeMVar send_mvar
  touchReq reqMsg
  withForeignPtr send_result $ \ptr -> do
    statusi <- (#peek FinishedRequest, status) ptr :: IO CInt
    case statusi of
      (#const SEND_ERROR) -> do
        msg <- peekFinishedRequestMsg ptr
        catchAndLog $ sendCob $ Just $ ChannelException $
          "sendCob: " <> decodeUtf8 msg
        return False
      (#const SEND_SUCCESS) -> do
        catchAndLog $ sendCob Nothing
        return True
      _ -> do
        sendCob (Just (ChannelException
          ("sendCollector: unexpected status: " <> showt statusi)))
        return False
 where
  catchAndLog io =
    io `catchAll` \e -> logError ("send callback threw: " ++ show e)


recvCollector :: MVar () -> ForeignPtr CFinishedRequest -> RecvCallback -> IO ()
recvCollector recv_mvar recv_result recvCob = do
  takeMVar recv_mvar
  withForeignPtr recv_result $ \ptr -> do
    statusi <- (#peek FinishedRequest, status) ptr :: IO CInt
    msg <- peekFinishedRequestMsg ptr
    catchAndLog $ case statusi of
      (#const RECV_ERROR) -> recvCob $ Left $ ChannelException $
                             "recvCob: " <> decodeUtf8 msg
      (#const RECV_SUCCESS) -> recvCob (Right (Response msg mempty))
      _ -> recvCob (Left (ChannelException
             ("recvCollector: unexpected status: " <> showt statusi)))
 where
  catchAndLog io =
     io `catchAll` \e -> logError ("recv callback threw: " ++ show e)

-- We need the send callback to touch the request message so that it doesn't get
-- garbage collected before the request is sent
touchReq :: ByteString -> IO ()
touchReq (PS fptr _ _) = touchForeignPtr fptr

-- The pieces we need to set up a callback from C to Haskell
newCallbackMVar :: IO (MVar (), StablePtr PrimMVar, ForeignPtr CFinishedRequest)
newCallbackMVar = do
  mvar <- newEmptyMVar
  sp <- newStablePtrPrimMVar mvar
  ptr <- mallocForeignPtrBytes (#const sizeof(FinishedRequest))
  return (mvar, sp, ptr)

data CFinishedRequest

-- Pack the message bytes into a ByteString that will call free when
-- it is garbage collected
peekFinishedRequestMsg :: Ptr CFinishedRequest -> IO ByteString
peekFinishedRequestMsg ptr =
  join $ curry unsafePackMallocCStringLen
            <$> (#peek FinishedRequest, buffer) ptr
            <*> (fromIntegral <$>
                   ((#peek FinishedRequest, len) ptr :: IO CSize))

--------------------------------------------------------------------------------

foreign import ccall unsafe "newWrapper"
  c_newWrapper
    :: Ptr CppRequestChannelPtr
    -> IO (Ptr CppWrappedChannel)

foreign import ccall unsafe "deleteWrapper"
  c_deleteWrapper
    :: Ptr CppWrappedChannel
    -> IO ()


foreign import ccall unsafe "getInnerRequestChannel"
  c_getInnerRequestChannel
    :: Ptr CppWrappedChannel
    -> IO (Ptr CppRequestChannelPtr)

-- This is implemented using runInEventBaseThread(), which is
-- non-blocking, so we can make this call unsafe.
foreign import ccall unsafe "sendReq"
  c_sendReq
    :: Ptr CppWrappedChannel
    -> CString
    -> CSize
    -> CInt
    -> StablePtr PrimMVar
    -> StablePtr PrimMVar
    -> Ptr CFinishedRequest
    -> Ptr CFinishedRequest
    -- RPC Options
    -> CString
    -> CSize
    -> IO ()

-- This is implemented using runInEventBaseThread(), which is
-- non-blocking, so we can make this call unsafe.
foreign import ccall unsafe "sendOnewayReq"
  c_sendOnewayReq
    :: Ptr CppWrappedChannel
    -> CString
    -> CSize
    -> CInt
    -> StablePtr PrimMVar
    -> Ptr CFinishedRequest
    -- RPC Options
    -> CString
    -> CSize
    -> IO ()
