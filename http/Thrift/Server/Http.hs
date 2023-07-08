{-# LANGUAGE TypeApplications #-}
module Thrift.Server.HTTP (
    ServerOptions(..),
    Server(..),
    defaultOptions,
    withBackgroundServer,
  ) where

import Control.Concurrent.Async
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal
import Network.Wai

import Thrift.Processor
import Thrift.Protocol.Binary

-- TODO:
--  - select protocol with header?
--  - do we have to do something to select body encoding?
--  - signal handling, background server?
--  - exceptions? result status?

data ServerOptions = ServerOptions
  { desiredPort :: Maybe Int
  , numWorkerThreads :: Maybe Int
  }

defaultOptions :: ServerOptions
defaultOptions = ServerOptions
  { desiredPort = Nothing
  , numWorkerThreads = Nothing
  }

data Server = Server
  { serverPort :: Int
  , serverAsync :: Async ()
  }

withBackgroundServer
  :: forall s a . (Processor s)
  => (forall r . s r -> IO r) -- ^ handler to use
  -> ServerOptions
  -> (Server -> IO a)
  -> IO a
withBackgroundServer handler ServerOptions{..} action = do
  withAsync (runSettings settings application) $ \a ->
    action (Server (settingsPort settings) a)
  where
  settings = maybe id setPort desiredPort defaultSettings
  application req respond = do
    body <- strictRequestBody req
    (res, _maybeEx) <- process (Proxy @Binary) 0 handler (LBS.toStrict body)
    respond $ responseLBS
      status200
      [("Content-Type", "application/x-thrift-binary")]
      (LBS.fromStrict res)
