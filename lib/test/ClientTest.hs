{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module ClientTest where

import Control.Concurrent
import Control.Exception hiding (DivideByZero)
import Control.Monad.Trans.Reader
import Data.Proxy
import TestRunner
import Test.HUnit hiding (State)

import Thrift.Channel
import Thrift.Monad
import Thrift.Protocol
import Thrift.Protocol.Binary

import Math.Calculator.Client

import TestCommon
import TestChannel

mkClientTestWith
  :: String -> RpcOptions -> ThriftM Binary TestChannel Calculator () -> Test
mkClientTestWith label opts action = TestLabel label $ TestCase $ do
  let proxy = Proxy :: Proxy Binary
  channel <- TestChannel <$> newEmptyMVar
  bracket (forkIO (runCalculatorServer proxy channel)) killThread $ const $ do
    counter <- newCounter
    let env = ThriftEnv proxy channel opts counter
    runReaderT action env

main :: IO ()
main = testRunner . TestList $
  runChannelTests mkClientTestWith
       [ addTest, divTest, putGetTest, putPutGetTest
       , exceptionTest, optionsTest, unimplementedTest, multiTest ]

-- Server Implementation -------------------------------------------------------

-- This is a library function
runCalculatorServer :: Protocol p => Proxy p -> TestChannel Calculator -> IO ()
runCalculatorServer proxy ch = do
  state  <- initServerState
  runServer proxy ch (processCommand state) (\_ _ -> [])
