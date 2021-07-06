-- Copyright (c) Facebook, Inc. and its affiliates.

module LifetimeTest (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Text (Text)

import Facebook.Init
import Network (testServerHost)
import Test.HUnit
import TestRunner

import Thrift.Api
import Thrift.Channel.HeaderChannel
import Thrift.Monad
import Thrift.Protocol.Id
import Thrift.Server.CppServer
import Util.EventBase

import Echoer.Echoer.Client
import EchoHandler

mkHeaderConfig :: Int -> HeaderConfig t
mkHeaderConfig port =
  HeaderConfig
    { headerHost = testServerHost
    , headerPort = port
    , headerProtocolId = compactProtocolId
    , headerConnTimeout = 5000
    , headerSendTimeout = 5000
    , headerRecvTimeout = 5000
    }

headerTest :: Test
headerTest = TestCase $
  withTestServer $ \port ->
    withEventBaseDataplane $ \evb -> do
      m <- newEmptyMVar
      withHeaderChannel evb (mkHeaderConfig port) $ request m
      -- channel is released, but the request should still complete
      takeMVar m >>= either throwIO print

withTestServer :: (Int -> IO a) -> IO a
withTestServer action = do
  st <- initEchoerState
  withBackgroundServer (echoHandler st) defaultOptions $
    \Server{..} -> action serverPort

request :: MVar (Either SomeException Text) -> Thrift Echoer ()
request m = do
 ThriftEnv{..} <- ask
 lift $ send_echo thriftProxy thriftChannel thriftCounter
   (maybe (return ()) (putMVar m . Left . toException))
   (either throwIO (putMVar m . recv_echo thriftProxy))
   thriftRpcOpts
   "message"

main :: IO ()
main = withFacebookUnitTest $
  testRunner $ TestList
    [ TestLabel "lifetime-header" headerTest
    ]
