-- Copyright (c) Facebook, Inc. and its affiliates.

module HandlerTest where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans.Class
import Data.Proxy
import Thrift.Monad
import Thrift.Processor
import Thrift.Protocol.Binary

import TestRunner
import Test.HUnit hiding (State)

import EchoHandler
import Echoer.Echoer.Client
import Math.Adder.Client
import TestChannel

mkTest :: String -> ThriftM Binary TestChannel Echoer () -> Test
mkTest label action = TestLabel label $ TestCase $ do
  channel <- TestChannel <$> newEmptyMVar :: IO (TestChannel Echoer)
  echoSt <- initEchoerState
  let handler = process proxy 0 (echoHandler echoSt)
  bracket (forkIO (runTestServer channel handler)) killThread $ const $
    runThrift action channel
  where
    proxy = Proxy :: Proxy Binary

echoTest :: Test
echoTest = mkTest "Echo" $ do
  res <- echo msg
  lift $ assertEqual "Echoer echoed" msg res
  where
    msg = "ace of spades"

addTest :: Test
addTest = mkTest "Add" $ do
  res <- add 4 5
  lift $ assertEqual "Add added" (4 + 5) res

main :: IO ()
main = testRunner $ TestList
  [ echoTest
  , addTest
  ]
