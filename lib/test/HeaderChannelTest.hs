-- Copyright (c) Facebook, Inc. and its affiliates.

module HeaderChannelTest where

import Control.Exception
import Control.Monad.Trans.Class
import Data.Proxy
import Foreign
import Foreign.C
import Network (testServerHost)
import Test.HUnit
import TestRunner
import Util.EventBase

import Thrift.Api
import Thrift.Channel
import Thrift.Channel.HeaderChannel
import Thrift.Monad
import Thrift.Protocol
import Thrift.Protocol.Id

import Math.Adder.Client
import Math.Calculator.Client

myRpcOptions :: RpcOptions
myRpcOptions = defaultRpcOptions { rpc_timeout = 5000 }

mySvcConf :: Int -> HeaderConfig Calculator
mySvcConf port = HeaderConfig
  { headerHost = testServerHost
  , headerPort = port
  , headerProtocolId = compactProtocolId
  , headerConnTimeout = 5000
  , headerSendTimeout = 5000
  , headerRecvTimeout = 5000
  }

mkChannelTest
  :: String
  -> Thrift Calculator ()
  -> Test
mkChannelTest label action = TestLabel label $ TestCase $
  withEventBaseDataplane $ \evb ->
    withMathServer $ \port -> withHeaderChannel evb (mySvcConf port) action

addTest :: Test
addTest = mkChannelTest "add test" $ do
  res <- withOptions myRpcOptions $ add 5 2
  lift $ assertEqual "5 + 2 = 7" 7 res

divideTest :: Test
divideTest = mkChannelTest "divide test" $ do
  res <- withOptions myRpcOptions $ divide 9 3
  lift $ assertEqual "9 / 3 = 3" 3 res

putAndGetTest :: Test
putAndGetTest = mkChannelTest "Get gets Put" $ do
  res <- withOptions myRpcOptions $ do
    put val
    get
  lift $ assertEqual "Get is same as Put" res val
  where
    val = 1337

-- Tests for IO variants of the thrift functions -----------------

mkChannelIOTest
  :: String
  -> (forall p . Protocol p =>
      Counter -> HeaderWrappedChannel Calculator -> Proxy p -> IO ())
  -> Test
mkChannelIOTest label action = TestLabel label $ TestCase $
  withEventBaseDataplane $ \evb ->
    withMathServer $ \port -> do
      counter <- newCounter
      withHeaderChannelIO evb (mySvcConf port) $ action counter

addIOTest :: Test
addIOTest = mkChannelIOTest "addIO test" $ \counter c p  -> do
  res <- addIO p c counter myRpcOptions 5 2
  assertEqual "5 + 2 = 7" 7 res

divideIOTest :: Test
divideIOTest = mkChannelIOTest "divideIO test" $ \counter c p  -> do
  res <- divideIO p c counter myRpcOptions 9 3
  assertEqual "9 / 3 = 3" 3 res

putAndGetIOTest :: Test
putAndGetIOTest = mkChannelIOTest "GetIO gets PutIO" $ \counter c p -> do
  putIO p c counter myRpcOptions val
  res <- getIO p c counter myRpcOptions
  assertEqual "Get is same as Put" res val
  where
    val = 1337


main :: IO ()
main = testRunner $ TestList
  [ addTest
  , divideTest
  , putAndGetTest
  , addIOTest
  , divideIOTest
  , putAndGetIOTest
  ]

-- -----------------------------------------------------------------------------

withMathServer :: (Int -> IO a) -> IO a
withMathServer action =  bracket makeCPPServer destroyCPPServer $ \s -> do
  p <- getPort s
  action $ fromIntegral p

data Server

foreign import ccall safe "make_cpp_server"
  makeCPPServer :: IO (Ptr Server)

foreign import ccall unsafe "get_port_from_server"
  getPort :: Ptr Server -> IO CInt

foreign import ccall safe "destroy_cpp_server"
  destroyCPPServer :: Ptr Server -> IO ()
