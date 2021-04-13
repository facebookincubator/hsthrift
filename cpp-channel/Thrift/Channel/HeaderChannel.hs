-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE StandaloneDeriving #-}
module Thrift.Channel.HeaderChannel
  ( HeaderWrappedChannel
  , HeaderConfig(..)
  , getRequestChannelProtocolId
  , withHeaderChannel
  , withHeaderChannelIO
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Data.Proxy
import Foreign.C
import Foreign.Ptr

import Thrift.Channel.CppChannel
import Thrift.Monad
import Thrift.Protocol
import Thrift.Protocol.Id
import Util.EventBase

data HeaderClient
type HeaderWrappedChannel = WrappedChannel HeaderClient

data HeaderConfig a = HeaderConfig
  { headerHost :: ByteString
  , headerPort :: Int
  , headerProtocolId :: ProtocolId
  , headerConnTimeout :: Int
  , headerSendTimeout :: Int
  , headerRecvTimeout :: Int
  }

deriving instance Show (HeaderConfig a)

withHeaderChannel
    :: EventBaseDataplane
    -> HeaderConfig t
    -> (forall p . Protocol p => ThriftM p HeaderWrappedChannel t a)
    -> IO a
withHeaderChannel io HeaderConfig{..} fn = do
  eb <- getEventBase io
  BS.unsafeUseAsCStringLen headerHost $ \(host_str, host_len) -> bracket
    (newHeaderChannel host_str (fromIntegral host_len)
                      (fromIntegral headerPort)
                      headerProtocolId
                      (fromIntegral headerConnTimeout)
                      (fromIntegral headerSendTimeout)
                      (fromIntegral headerRecvTimeout)
                      eb
                      )
    deleteHeaderChannel $ \ch -> do
      protId <- getRequestChannelProtocolId ch
      withProxy protId $ runAction ch fn
  where
    runAction
      :: Protocol p
      => Ptr CppRequestChannelPtr
      -> ThriftM p HeaderWrappedChannel t a
      -> Proxy p
      -> IO a
    runAction c a _ = withCppChannel c a

withHeaderChannelIO
    :: EventBaseDataplane
    -> HeaderConfig t
    -> (forall p . Protocol p => HeaderWrappedChannel t -> Proxy p -> IO a)
    -> IO a
withHeaderChannelIO io HeaderConfig{..} fn = do
  eb <- getEventBase io
  BS.unsafeUseAsCStringLen headerHost $ \(host_str, host_len) -> bracket
    (newHeaderChannel host_str (fromIntegral host_len)
                      (fromIntegral headerPort)
                      headerProtocolId
                      (fromIntegral headerConnTimeout)
                      (fromIntegral headerSendTimeout)
                      (fromIntegral headerRecvTimeout)
                      eb
                      )
    deleteHeaderChannel $ \ch -> do
      protId <- getRequestChannelProtocolId ch
      withCppChannelIO ch $ \ wr -> do
      withProxy protId $ fn wr


foreign import ccall safe "newHeaderChannel"
  newHeaderChannel :: CString -> CSize
                  -> CSize
                  -> CUShort
                  -> CSize
                  -> CSize
                  -> CSize
                  -> EventBase
                  -> IO (Ptr CppRequestChannelPtr)

foreign import ccall safe "deleteHeaderChannel"
  deleteHeaderChannel :: Ptr CppRequestChannelPtr -> IO ()

foreign import ccall unsafe "getRequestChannelProtocolId"
  getRequestChannelProtocolId :: Ptr CppRequestChannelPtr -> IO CUShort
