{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Support for creating a Thrift-over-HTTP server

{-# LANGUAGE TypeApplications #-}
module Thrift.Server.HTTP (
    ServerOptions(..),
    Server(..),
    defaultOptions,
    withBackgroundServer,
    withBackgroundServer',
    thriftApplication,
    Middleware,
  ) where

import Data.ByteString (ByteString)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Data.String
import Data.Streaming.Network (bindPortTCP, bindRandomPortTCP)
import Network.HTTP.Types
import Network.Socket (close)
import Network.Wai.Handler.Warp
import Network.Wai
  ( Application
  , Middleware
  , requestHeaders
  , responseLBS
  , strictRequestBody
  )

import Thrift.Processor hiding (Header)
import qualified Thrift.Processor as Thrift
import Thrift.Protocol
import Thrift.Protocol.Binary
import Thrift.Protocol.Compact
import Thrift.Protocol.JSON

-- TODO:
--  - one-way requests are currently treated as 2-way. Can we do any
--    better?

-- | Options for creating a Thrift-over-HTTP service.
data ServerOptions = ServerOptions
  { desiredPort :: Maybe Int
     -- ^ If 'Nothing', creates the server on a random free port,
     -- passing the actual port number as 'serverPort'.
  , numWorkerThreads :: Maybe Int
     -- ^ Currently ignored, provided for compatibility with CppServer
  , warpSettings :: Settings
  , middleware :: Middleware
  }

-- | Default options for creating a Thrift-over-HTTP service.
defaultOptions :: ServerOptions
defaultOptions = ServerOptions
  { desiredPort = Nothing
  , numWorkerThreads = Nothing
  , warpSettings = setHost (fromString "!6") defaultSettings
      -- IPv6 only by default
  , middleware = id
  }

-- | A running HTTP server.
data Server = Server
  { serverPort :: Int
      -- The actual port number, which might be useful if
      -- 'desiredPort' was 'Nothing'.
  , serverAsync :: Async ()
      -- The 'Async' running the HTTP server
  }

-- | Create an HTTP server for a Thrift service from the given 'ServerOptions'.
-- This is a simple wrapper around Warp's 'runSettings' that optionally creates
-- the server on a random port, and also wait for the server to start before
-- invoking the given action. Shuts down the server when the action returns.
withBackgroundServer
  :: forall s a . (Processor s)
  => (forall r . s r -> IO r) -- ^ handler to use
  -> ServerOptions
  -> (Server -> IO a)  -- ^ action to run while the server is up
  -> IO a
withBackgroundServer handler = withBackgroundServer' handler (\_ _ -> [])

-- | Create an HTTP server for a Thrift service from the given 'ServerOptions'.
-- This is a simple wrapper around Warp's 'runSettings' that optionally creates
-- the server on a random port, and also wait for the server to start before
-- invoking the given action. Shuts down the server when the action returns.
withBackgroundServer'
  :: forall s a . (Processor s)
  => (forall r . s r -> IO r) -- ^ handler to use
  -> (forall r . s r -> Either SomeException r -> Thrift.Header)
  -> ServerOptions
  -> (Server -> IO a)  -- ^ action to run while the server is up
  -> IO a
withBackgroundServer' handler postProcess ServerOptions{..} action = do
  ready <- newEmptyMVar
  let
    host = getHost warpSettings
    application = middleware (thriftApplication handler postProcess)

    settings =
      maybe id setPort desiredPort $
      setBeforeMainLoop (putMVar ready ())
      warpSettings

    go port sock =
      withAsync (runSettingsSocket settings sock application) $ \a -> do
        takeMVar ready
        action (Server port a)

  case desiredPort of
    Nothing ->
      bracket (bindRandomPortTCP host) (close . snd) $ \(port,sock) ->
        go port sock
    Just port ->
      bracket (bindPortTCP port host) close (go port)

-- | Make a WAI 'Application' for a Thrift service. Use this with a
-- transport layer such as Warp to make a complete server, or call
-- 'withBackgroundServer' to do it all.
thriftApplication
  :: forall s . (Processor s)
  => (forall r . s r -> IO r) -- ^ handler to use
  -> (forall r . s r -> Either SomeException r -> Thrift.Header)
  -> Application
thriftApplication handler postProcess req respond = do
  body <- strictRequestBody req
  withProto (requestHeaders req) $ \proto contentType -> do
    (res, _maybeEx, _headers) <-
      process proto 0 handler postProcess (LBS.toStrict body)
    respond $ responseLBS
      status200
      [(hContentType, contentType)]
      (LBS.fromStrict res)
  where
  withProto
   :: [Header]
   -> (forall p . Protocol p => Proxy p -> ByteString -> IO b)
   -> IO b
  withProto hdrs f =
    case [ t | (header,t) <- hdrs, header == hContentType ] of
      t@"application/x-thrift-binary" : _ -> f (Proxy @Binary) t
      t@"application/x-thrift-compact" : _ -> f (Proxy @Compact) t
      t@"application/x-thrift-json" : _ -> f (Proxy @JSON) t
      _ -> f (Proxy @Binary) "application/x-thrift-binary"
