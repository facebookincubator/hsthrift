{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Thrift.Channel.HTTP (
    HTTPConfig(..),
    withHTTPChannel,
    withHTTPChannelIO,
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Network.HTTP.Types

import Thrift.Channel
import Thrift.Monad
import Thrift.Protocol
import Thrift.Protocol.Id

data HTTPChannel s = HTTPChannel
  { httpConfig :: HTTPConfig s
  , httpManager :: Manager
  }

data HTTPConfig s = HTTPConfig
  { httpHost :: ByteString
  , httpPort :: Int
  , httpProtocolId :: ProtocolId
  , httpUseHttps :: Bool
  , httpResponseTimeout :: Maybe Int -- ^ microseconds
  }
  deriving Show

withHTTPChannel
    :: HTTPConfig t
    -> (forall p . Protocol p => ThriftM p HTTPChannel t a)
    -> IO a
withHTTPChannel config@HTTPConfig{..} action = do
  manager <- newTlsManagerWith tlsManagerSettings {
    managerResponseTimeout =
      maybe responseTimeoutNone responseTimeoutMicro httpResponseTimeout }
  withProxy httpProtocolId $ \proxy ->
    runAction (HTTPChannel config manager) action proxy
  where
    runAction
      :: Protocol p
      => HTTPChannel t
      -> ThriftM p HTTPChannel t a
      -> Proxy p
      -> IO a
    runAction c a _ = runThrift a c

withHTTPChannelIO
    :: HTTPConfig t
    -> (forall p . Protocol p => HTTPChannel t -> Proxy p -> IO a)
    -> IO a
withHTTPChannelIO config@HTTPConfig{..} action = do
  manager <- newTlsManagerWith tlsManagerSettings
  withProxy httpProtocolId $ \proxy ->
    action (HTTPChannel config manager) proxy

instance ClientChannel HTTPChannel where
  sendRequest = httpRequest
  sendOnewayRequest chan req sendCb = httpRequest chan req sendCb (\_ -> return ())

httpRequest
  :: HTTPChannel t
  -> Thrift.Channel.Request
  -> SendCallback
  -> RecvCallback
  -> IO ()
httpRequest HTTPChannel{..} Request{..} sendCb recvCb = do
    let !prot = httpProtocolId httpConfig
    let request = defaultRequest
          { host = httpHost httpConfig
          , port = httpPort httpConfig
          , method = "POST"
          , secure = httpUseHttps httpConfig
          , requestHeaders =
              [ (hContentType, if
                  | prot == binaryProtocolId ->
                      "application/x-thrift-binary"
                  | prot == compactProtocolId ->
                      "application/x-thrift-compact"
                  | otherwise -> -- later: JSON
                      "application/x-thrift-binary") ]
          , requestBody = RequestBodyBS reqMsg
          }
          -- TODO: rpcOpts
    response <- httpLbs request httpManager
    sendCb Nothing -- TODO?
    let s = responseStatus response
    if s == status200 then
      recvCb $ Right Response
        { respMsg = LBS.toStrict (responseBody response)
        , respHeader = [] }
    else
      recvCb (Left (ChannelException (Text.decodeUtf8 (statusMessage s))))
