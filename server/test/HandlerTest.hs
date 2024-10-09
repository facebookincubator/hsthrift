{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module HandlerTest where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (mapReaderT)
import Data.Proxy
import qualified Data.Text as Text
import Thrift.Monad
import Thrift.Processor
import Thrift.Protocol.ApplicationException.Types
import Thrift.Protocol.Binary

import TestRunner
import Test.HUnit hiding (State)

import EchoHandler
import Echoer.Echoer.Client
import Math.Adder.Client
import Math.Calculator.Client as Calculator
import Math.Types (QuotRemResponse(..))
import TestChannel

mkTest :: String -> ThriftM Binary TestChannel Echoer () -> Test
mkTest label action = TestLabel label $ TestCase $ do
  channel <- TestChannel <$> newEmptyMVar :: IO (TestChannel Echoer)
  echoSt <- initEchoerState
  let handler = process proxy 0 (echoHandler echoSt) (\_ _ -> [])
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

quotRemTest :: Test
quotRemTest = mkTest "QuotRem" $ do
  res <- Calculator.quotRem 7 3
  lift $ assertEqual "QuotRem" (QuotRemResponse 2 1) res

quotRemByZeroTest :: Test
quotRemByZeroTest = mkTest "QuotRemByZero" $ do
  res <- mapReaderT try $ Calculator.quotRem 1 0
  lift $ case res of
    Left e | Just ApplicationException{..} <- fromException e -> do
      assertEqual "message" (Text.pack $ show DivideByZero)
        applicationException_message
      assertEqual "type" ApplicationExceptionType_InternalError
        applicationException_type
    _ -> assertFailure $ "An ApplicationException is expected, got " ++ show res

main :: IO ()
main = testRunner $ TestList
  [ echoTest
  , addTest
  , quotRemTest
  , quotRemByZeroTest
  ]
