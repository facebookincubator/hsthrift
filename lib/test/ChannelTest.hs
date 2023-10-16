{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module ChannelTest where

import Control.Concurrent
import Control.Exception hiding (handle)
import Test.HUnit
import TestRunner

import TestChannel
import Thrift.Channel

clientTest :: Test
clientTest = TestLabel "client test" $ TestCase $ do
  channel <- TestChannel <$> newEmptyMVar
  bracket
    (forkIO $ runTestServer channel $ \x -> return (x, Nothing, []))
    killThread $
    const $ do
    let request = "this is my one request"
    (handle, sendCob, recvCob) <- mkCallbacks Right
    sendRequest channel (simpleRequest request) sendCob recvCob
    Response{..} <- wait handle
    assertEqual "request is same as response" request respMsg

main :: IO ()
main = testRunner clientTest
