module SocketChannelTest where

import Control.Concurrent
import Control.Exception hiding (DivideByZero)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Data.Proxy
import TestRunner
import Test.HUnit hiding (State)

import Thrift.Api
import Thrift.Channel
import Thrift.Channel.SocketChannel as Sock
import Thrift.Monad
import Thrift.Protocol
import Thrift.Protocol.ApplicationException.Types
import Thrift.Protocol.Binary
import Thrift.Protocol.Id

import Math.Types
import Math.Adder.Client
import Math.Adder.Service
import Math.Calculator.Client
import Math.Calculator.Service

import SocketServer
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
mkClientTestSockWith lbl opts action = TestLabel lbl $ TestCase $ do
  state <- initServerState
  withServer binaryProtocolId (processCommand state) action
