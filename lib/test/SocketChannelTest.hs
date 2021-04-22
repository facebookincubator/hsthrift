-- Copyright (c) Facebook, Inc. and its affiliates.

module SocketChannelTest where

import Network.Socket (maxListenQueue)
import TestRunner
import Test.HUnit hiding (State)

import Thrift.Api
import Thrift.Channel
import Thrift.Channel.SocketChannel.Server
import Thrift.Protocol.Id

import Math.Calculator.Client

import TestCommon

main :: IO ()
main = testRunner . TestList $
  runChannelTests mkClientTestSockWith
       [ addTest, divTest, putGetTest, putPutGetTest
       , exceptionTest, unimplementedTest, multiTest ]

-- Client utilities ------------------------------------------------------------

mkClientTestSock
  :: String
  -> Thrift Calculator ()
  -> Test
mkClientTestSock lbl = mkClientTestSockWith lbl defaultRpcOptions

mkClientTestSockWith
  :: String
  -> RpcOptions
  -> Thrift Calculator ()
  -> Test
mkClientTestSockWith lbl _opts action = TestLabel lbl $ TestCase $ do
  state <- initServerState
  withServer binaryProtocolId Nothing maxListenQueue (processCommand state) action
