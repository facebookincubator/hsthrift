module ClientTest where

import Control.Concurrent
import Control.Exception hiding (DivideByZero)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Data.Proxy
import TestRunner
import Test.HUnit hiding (State)

import Math.Types
import Math.Adder.Client
import Math.Adder.Service
import Math.Calculator.Client
import Math.Calculator.Service

import TestChannel
import Thrift.Channel
import Thrift.Monad
import Thrift.Protocol
import Thrift.Protocol.ApplicationException.Types
import Thrift.Protocol.Binary

mkClientTest :: String -> ThriftM Binary TestChannel Calculator () -> Test
mkClientTest label =
  mkClientTestWith label defaultRpcOptions

mkClientTestWith
  :: String -> RpcOptions -> ThriftM Binary TestChannel Calculator () -> Test
mkClientTestWith label opts action = TestLabel label $ TestCase $ do
  let proxy = Proxy :: Proxy Binary
  channel <- TestChannel <$> newEmptyMVar
  bracket (forkIO (runCalculatorServer proxy channel)) killThread $ const $ do
    counter <- newCounter
    let env = ThriftEnv proxy channel opts counter
    runReaderT action env

addTest :: Test
addTest = mkClientTest "add test" $ do
  result <- add 5 2
  lift $ assertEqual "5 + 2 = 7" 7 result

divTest :: Test
divTest = mkClientTest "divide test" $ do
  result <- divide 10 2
  lift $ assertEqual "10 / 2 = 5" 5 result

putGetTest :: Test
putGetTest = mkClientTest "put get" $ do
  let value = 99
  put value
  result <- get
  lift $ assertEqual "put 99 = get" value result

exceptionTest :: Test
exceptionTest = mkClientTest "exception test" $
  (void . lift . evaluate =<< divide 0 0)
    `catchThrift` \DivideByZero -> return ()

optionsTest :: Test
optionsTest = mkClientTestWith "options test" opts $
  (void . lift . evaluate =<< (add 0 0 *> error "fail"))
    `catchThrift` \ChannelException{} -> return ()
  where
    opts = setRpcPriority defaultRpcOptions High

unimplementedTest :: Test
unimplementedTest = mkClientTest "unimplemented test" $
  unimplemented `catchThrift` \ApplicationException{} -> return ()

multiTest :: Test
multiTest = mkClientTest "multiple requests" $ do
  r1 <- add 2 2
  lift $ assertEqual "2 + 2 = 4" 4 r1

  r2 <- divide 64 16
  lift $ assertEqual "64 / 16 = 4" 4 r2

  put 100
  r3 <- get
  lift $ assertEqual "put = get" 100 r3

main :: IO ()
main = testRunner $ TestList
       [ addTest, divTest, putGetTest
       , exceptionTest, optionsTest, unimplementedTest, multiTest ]

-- Server Implementation -------------------------------------------------------

-- This is a library function
runCalculatorServer :: Protocol p => Proxy p -> TestChannel Calculator -> IO ()
runCalculatorServer proxy ch = do
  state  <- initServerState
  runServer proxy ch $ processCommand state

-- Server Implementation
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
