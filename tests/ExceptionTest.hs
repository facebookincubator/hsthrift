{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module ExceptionTest where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Trans.Class
import Test.HUnit
import TestChannel
import TestRunner

import Exception.Types
import Thrift.Monad

runDangerousCode :: IO ()
runDangerousCode = throw $ X "just because"

exceptionTest :: Test
exceptionTest = TestLabel "exception test" $ TestCase $
  runDangerousCode `catch` \X{} -> return ()

data Event = Acquire | Release | RunBody | Unreachable | Catch
  deriving (Eq, Show)

bracketTest :: Test
bracketTest = TestLabel "bracket test" $ TestCase $ do
  mvar <- newMVar []
  let logEvent ev = modifyMVar_ mvar (return . (ev:))
  channel <- TestChannel <$> newEmptyMVar
  runThrift
    (bracketThrift_
      (lift $ logEvent Acquire)
      (lift $ logEvent Release)
      (lift $ do
        logEvent RunBody
        runDangerousCode
        logEvent Unreachable))
    channel
    `catch` (\ X{} -> logEvent Catch)
  eventlog <- readMVar mvar
  assertEqual "eventlog"
    [Acquire, RunBody, Release, Catch]
    (reverse eventlog)

main :: IO ()
main = testRunner $ TestList
  [ exceptionTest
  , bracketTest ]
