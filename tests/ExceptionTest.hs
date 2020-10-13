--
-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements. See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership. The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- License); you may not use this file except in compliance
-- with the License. You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing,
-- software distributed under the License is distributed on an
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied. See the License for the
-- specific language governing permissions and limitations
-- under the License.
--

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
