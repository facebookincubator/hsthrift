-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
module Thrift.Server.CppServer
  ( Server(..)
  , withBackgroundServer

  , module Thrift.Server.Types
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Maybe
#if __GLASGOW_HASKELL == 804
import Data.Monoid ((<>))
#endif
import Data.Proxy
import Data.Text (Text)
import Foreign
import Foreign.C
import Foreign.CPP.Marshallable.TH
import Foreign.CPP.HsStruct
import GHC.Event (Lifetime(OneShot))
import System.Posix.Types
import Util.Fd
import Util.Text
import Thrift.Server.ProcessorCallback
import Thrift.Server.Types
import Thrift.Processor

-- -----------------------------------------------------------------------------
-- Server data types

data CServer
type PServer = Ptr CServer

$(deriveDestructibleUnsafe "CreateCppServerResult" [t| HsEither PServer HsText |])

data Server = Server
  { pServer :: PServer
  , serverPort :: Int
  }

-- -----------------------------------------------------------------------------
-- Down to business

-- | Spawns a background thread that blocks on the server
-- Creates a server in a background thread to serve requests. After the server
-- has been successfully started, the supplied Server -> IO a action is
-- executed, and when this action returns the server is  terminated. A pool of
-- worker threads executes requests by calling the s r -> IO r action for each
-- request received.
withBackgroundServer :: forall s a . (Processor s)
                     => (forall r . s r -> IO r) -- ^ handler to use
                     -> ServerOptions
                     -> (Server -> IO a)
                     -> IO a
withBackgroundServer handler ServerOptions{..} action =
  withProcessorCallback handler runServer
  where
    err = ServerException "failed to get event manager"
    cPort = fromIntegral $ fromMaybe 0 desiredPort

    -- Use the normal factory unless a custom was handed to us
    factoryFn = fromMaybe c_haskell_factory customFactoryFn

    -- Function to modify the ThriftServer
    modifyFn = fromMaybe nullFunPtr customModifyFn

    -- Set the instance to pull oneway functions from
    oneways = onewayFns (undefined :: Proxy s)

    throwEx prefix = throwIO . ServerException . ((prefix <> ": ") <>)

    -- Creates a PServer to run `act` on
    withCServer cb act =
      useTextsAsCStringLens oneways $ \txts sizes txtlen -> do
        let
          alloc = bracket
            (create_cpp_server cb factoryFn cPort txts sizes txtlen)
            delete
            $ \p -> do
              (r :: Either PServer Text) <- coerce $ peek p
              case r of
                Left ps -> return ps
                Right exStr -> throwEx "create_cpp_server" exStr
        bracket alloc destroy_cpp_server act

    runServer cb =
      withCServer cb $ \ps ->
      alloca $ \portPtr -> do
        mvar <- newEmptyMVar
        let callback = putMVar mvar =<< peek portPtr
        withFdEventNotification err callback OneShot $ \(Fd cfd) ->
          let
            start =
              bracket (c_serve ps cfd portPtr modifyFn) delete $ \ptr ->
                when (ptr /= nullPtr) $ do
                  (exStr :: Text) <- coerce <$> peek ptr
                  throwEx "cpp_server" exStr
            stop a = c_stop ps >> wait a
          in
            bracket (async start) stop $ \_ -> do
              port <- takeMVar mvar -- Blocks until serving or exception
              when (port == 0) (throwEx "cpp_server" "this should never happen")
              action Server
                { pServer = ps
                , serverPort = fromIntegral port
                }

-- -----------------------------------------------------------------------------
-- FFI

foreign import ccall safe "c_create_cpp_server"
  create_cpp_server :: FunPtr ProcessorCallback
                    -> FactoryFunction
                    -> CInt -- ^ port
                    -> Ptr CString -- ^ oneway function names array
                    -> Ptr CSize -- ^ oneway function name lengths array
                    -> CSize -- ^ number of oneway functions
                    -> IO (Ptr (HsEither PServer HsText))

foreign import ccall safe "c_destroy_cpp_server"
  destroy_cpp_server :: PServer -> IO ()

foreign import ccall safe "c_serve_cpp_server"
  c_serve :: PServer
          -> CInt  -- ^ file descriptor to write to
          -> Ptr CInt  -- ^ place to put value of port
          -> ModifyFunction
          -> IO (Ptr HsText)

foreign import ccall safe "c_stop_cpp_server"
  c_stop :: PServer -> IO ()

foreign import ccall "&c_haskell_factory"
  c_haskell_factory :: FactoryFunction
