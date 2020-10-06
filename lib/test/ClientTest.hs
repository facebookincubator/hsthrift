module ClientTest where

import Control.Concurrent
import Control.Exception hiding (DivideByZero)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Thrift.Binary.Parser
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict)
import Data.Int
import Data.IORef
import Data.Proxy
import Data.Some
import Data.Text (Text)
import TestRunner
import Test.HUnit hiding (State)
import qualified Data.Text as Text

import Math.Types
import Math.Adder.Client
import Math.Calculator.Client

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
  bracket (forkIO (runServer proxy channel)) killThread $ const $ do
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
runServer :: Protocol p => Proxy p -> TestChannel Calculator -> IO ()
runServer proxy (TestChannel req) = do
  counter <- newCounter
  state   <- initServerState
  forever $ do
    -- Get the request (needs to be abstracted using ServerChannel type class)
    Req bytes cob <- takeMVar req
    seqNum <- counter
    -- Parse the message
    case parse (msgParser proxy) bytes of
      -- Parse failure
      Left err -> putBuilder cob $
        genMsgBegin proxy "" 3 seqNum <>
        buildStruct proxy
        (ApplicationException (Text.pack err)
         ApplicationExceptionType_ProtocolError)  <>
        genMsgEnd proxy
      -- Process the command
      Right (This command) ->
        sendResult proxy cob seqNum (processCommand state) command

-- This will be generated code
data Command a where
  Add :: Int64 -> Int64 -> Command Int64
  Divide :: Double -> Double -> Command DivideResult
  Put :: Int64 -> Command ()
  Get :: Command Int64
  Unimplemented :: Command ()

-- anything that throws should be marked as such
data DivideResult = DivideResult Double | Divide_DivideByZero DivideByZero

getName :: Command a -> Text
getName Add{} = "add"
getName Divide{} = "divide"
getName Put{} = "put"
getName Get = "get"
getName Unimplemented = "unimplemented"

msgParser :: Protocol p => Proxy p -> Parser (Some Command)
msgParser proxy = do
  MsgBegin funName _ _ <- parseMsgBegin proxy
  command <-
    case funName of
     "add" -> fmap This $ Add
       <$> (parseFieldBegin proxy 0 mempty *> parseI64 proxy)
       <*> (parseFieldBegin proxy 0 mempty *> parseI64 proxy)
     "divide" -> fmap This $ Divide
       <$> (parseFieldBegin proxy 0 mempty *> parseDouble proxy)
       <*> (parseFieldBegin proxy 0 mempty *> parseDouble proxy)
     "put" -> This . Put
       <$> (parseFieldBegin proxy 0 mempty *> parseI64 proxy)
     "get" -> pure $ This Get
     "unimplemented" -> pure $ This Unimplemented
     _ -> fail $ "unknown function call: " ++ Text.unpack funName

  _ <- parseFieldBegin proxy 0 mempty *> parseMsgEnd proxy
  return command

sendResult
  :: Protocol p
  => Proxy p
  -> RecvCallback
  -> Int32
  -> (Command a -> IO a)
  -> Command a
  -> IO ()
sendResult proxy cob seqNum process command = do
  result <- try $ process command
  case result of
    Left e -> putBuilder cob $
      genMsgBegin proxy (getName command) 3 seqNum <>
      buildStruct proxy
      (case fromException e :: Maybe ApplicationException of
         Just ex -> ex
         Nothing -> ApplicationException (Text.pack (show e))
                    ApplicationExceptionType_InternalError) <>
      genMsgEnd proxy
    Right r ->
      case command of
        Add{} -> putBuilder cob $
          genMsgBegin proxy "add" 2 seqNum <>
          genStruct proxy
          [ genField proxy "" (getI64Type proxy) 0 0 (genI64 proxy r) ] <>
          genMsgEnd proxy
        Divide{} -> putBuilder cob $
          genMsgBegin proxy "divide" 2 seqNum <>
          genStruct proxy
          [ case r of
              DivideResult x ->
                genField proxy "" (getDoubleType proxy) 0 0 $
                genDouble proxy x
              Divide_DivideByZero e ->
                genField proxy "" (getStructType proxy) 1 0 $
                buildStruct proxy e
          ] <>
          genMsgEnd proxy
        Put{} -> return ()
        Get -> putBuilder cob $
          genMsgBegin proxy "get" 2 seqNum <>
          genStruct proxy
          [ genField proxy "" (getI64Type proxy) 0 0 (genI64 proxy r) ] <>
          genMsgEnd proxy
        Unimplemented -> putBuilder cob $
          genMsgBegin proxy "unimplemented" 2 seqNum <>
          genStruct proxy [] <>
          genMsgEnd proxy

putBuilder :: RecvCallback -> Builder -> IO ()
putBuilder cob b = cob $ Right $ Response (toStrict (toLazyByteString b)) mempty

-- Server Implementation
type State = IORef Int64

initServerState :: IO State
initServerState = newIORef 0

processCommand :: State -> Command a -> IO a
processCommand _ (Add x y) = pure $ x + y
processCommand _ (Divide x y)
  | y == 0    = pure $ Divide_DivideByZero DivideByZero
  | otherwise = pure $ DivideResult $ x / y
processCommand state (Put x) = writeIORef state x
processCommand state Get = readIORef state
processCommand _ Unimplemented =
  throw $ ApplicationException "" ApplicationExceptionType_UnknownMethod
