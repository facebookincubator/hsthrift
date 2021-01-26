-- Copyright (c) Facebook, Inc. and its affiliates.

module TestCommon where

import Control.Exception (throw, throwIO, evaluate)
import Control.Monad
import Control.Monad.Trans.Class
import Data.IORef
import Test.HUnit hiding (State)

import Thrift.Api
import Thrift.Channel
import Thrift.Monad
import Thrift.Protocol.ApplicationException.Types

import Math.Types
import Math.Adder.Client
import Math.Adder.Service
import Math.Calculator.Client
import Math.Calculator.Service

-- Server Implementation for the 'Calculator' service

type State = IORef Int

initServerState :: IO State
initServerState = newIORef 0

processCommand :: State -> CalculatorCommand a -> IO a
processCommand _ (SuperAdder (Add x y)) = pure $ x + y
processCommand _ (Divide x y)
  | y == 0    = throwIO DivideByZero
  | otherwise = pure $ x / y
processCommand state (Put x) = writeIORef state x
processCommand state (PutMany xs) = mapM_ (writeIORef state) xs
processCommand state Get = readIORef state
processCommand _ Unimplemented =
  throw $ ApplicationException "" ApplicationExceptionType_UnknownMethod


-- common client computations/tests used with diffeerent channel implementations

-- if needed, this 'Calculator' could just become a type
-- parameter, in case such utilities are needed for other
-- test suites.
data ChannelTest = ChannelTest
  { ctestName :: String
  , ctestOpts :: RpcOptions
  , ctestAct  :: Thrift Calculator ()
  }

-- | Package up a test label and the corresponding client
--   computation, to later be executed against a specific
--   channel implementation and server.
channelTest
  :: String -> Thrift Calculator () -> ChannelTest
channelTest lbl comp = channelTestWithOpts lbl defaultRpcOptions comp

-- | Like 'channelTest', but with the ability to pass 'RpcOptions' to the
--   test.
channelTestWithOpts
  :: String -> RpcOptions -> Thrift Calculator () -> ChannelTest
channelTestWithOpts = ChannelTest

runChannelTests
  :: (String -> RpcOptions -> Thrift Calculator () -> Test)
  -> [ChannelTest]
  -> [Test]
runChannelTests testFun tests =
  map (\ChannelTest{..} -> testFun ctestName ctestOpts ctestAct) tests

addTest :: ChannelTest
addTest = channelTest "add test" $ do
  result <- add 5 2
  lift $ assertEqual "5 + 2 = 7" 7 result

divTest :: ChannelTest
divTest = channelTest "divide test" $ do
  result <- divide 10 2
  lift $ assertEqual "10 / 2 = 5" 5 result

putGetTest :: ChannelTest
putGetTest = channelTest "put get" $ do
  let value = 99
  put value
  result <- get
  lift $ assertEqual "put 99 = get" value result

putPutGetTest :: ChannelTest
putPutGetTest = channelTest "put put get" $ do
  let value1 = 99
      value2 = 100
  put value1
  put value2
  result <- get
  lift $ assertEqual "put 99 then put 100 = get (= put 100)" value2 result

exceptionTest :: ChannelTest
exceptionTest = channelTest "exception test" $
  (void . lift . evaluate =<< divide 0 0)
    `catchThrift` \DivideByZero -> return ()

optionsTest :: ChannelTest
optionsTest = channelTestWithOpts "options test" opts $
  (void . lift . evaluate =<< (add 0 0 *> error "fail"))
    `catchThrift` \ChannelException{} -> return ()
  where
    opts = setRpcPriority defaultRpcOptions High

unimplementedTest :: ChannelTest
unimplementedTest = channelTest "unimplemented test" $
  unimplemented `catchThrift` \ApplicationException{} -> return ()

multiTest :: ChannelTest
multiTest = channelTest "multiple requests" $ do
  r1 <- add 2 2
  lift $ assertEqual "2 + 2 = 4" 4 r1

  r2 <- divide 64 16
  lift $ assertEqual "64 / 16 = 4" 4 r2

  put 100
  r3 <- get
  lift $ assertEqual "put = get" 100 r3
