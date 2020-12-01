-- Copyright (c) Facebook, Inc. and its affiliates.

module ServerTest (main) where

import Control.Exception hiding (DivideByZero)
import Control.Monad
import Control.Monad.Trans.Class
import Data.Either

import Facebook.Init
import Network (testServerHost)
import Test.HUnit
import TestRunner

import Thrift.Api
import Thrift.Channel.HeaderChannel
import Thrift.Monad
import Thrift.Protocol.ApplicationException.Types
import Thrift.Protocol.Id
import Util.EventBase

import Math.Adder.Client
import Math.Calculator.Client
import Math.Types
import Echoer.Echoer.Client
import EchoHandler
import Thrift.Server.CppServer

withTestServer :: ServerOptions -> (Int -> IO a) -> IO a
withTestServer serverOptions action = do
  st <- initEchoerState
  withBackgroundServer (echoHandler st) serverOptions $
    \Server{..} -> action serverPort

mkHeaderConfig :: Int -> ProtocolId -> HeaderConfig t
mkHeaderConfig port protId =
  HeaderConfig
    { headerHost = testServerHost
    , headerPort = port
    , headerProtocolId = protId
    , headerConnTimeout = 5000
    , headerSendTimeout = 5000
    , headerRecvTimeout = 5000
    }

mkServerTest
  :: String
  -> String
  -> ProtocolId
  -> Thrift Echoer ()
  -> Test
mkServerTest pname label protId action =
  TestLabel (pname ++ " " ++ label) $ TestCase $
    withTestServer defaultOptions $ \port ->
      withEventBaseDataplane $ \evb -> do
        let
          headerConf = mkHeaderConfig port protId
        withHeaderChannel evb headerConf action

-- Calculator function
addTest :: String -> ProtocolId -> Test
addTest pname protId = mkServerTest pname "add test" protId $ do
  res <- add 5 2
  lift $ assertEqual "5 + 2 = 7" 7 res

-- Calculator function
divideTest :: String -> ProtocolId -> Test
divideTest pname protId = mkServerTest pname "divide test" protId $ do
  res <- divide 9 3
  lift $ assertEqual "9 / 3 = 3" 3 res

divideExceptionTest :: String -> ProtocolId -> Test
divideExceptionTest pname protId =
  mkServerTest pname "divide exception" protId $
  (void . lift . evaluate =<< divide 1 0)
    `catchThrift` \DivideByZero -> return ()

-- Calculator function
multiTest :: String -> ProtocolId -> Test
multiTest pname protId = mkServerTest pname "multiple requests" protId $ do
  put 100

  r1 <- add 2 2
  lift $ assertEqual "2 + 2 = 4" 4 r1

  r2 <- divide 64 16
  lift $ assertEqual "64 / 16 = 4" 4 r2

  r3 <- get
  lift $ assertEqual "put = get" 100 r3

  r4 <- divide 100 10
  lift $ assertEqual "100 / 10 = 10" 10 r4

unimplementedTest :: String -> ProtocolId -> Test
unimplementedTest pname protId =
  mkServerTest pname "unimplemented test" protId $
    unimplemented `catchThrift` \ApplicationException{} -> return ()

-- Echo function
echoTest :: String -> ProtocolId -> Test
echoTest pname protId = mkServerTest pname "echo" protId $ do
  res <- echo val
  lift $ assertEqual "echo echoed" val res
  where
    val = "AAAAAAAAAA_DO_NOT_DELETE"

portAlreadyBoundTest :: String -> ProtocolId -> Test
portAlreadyBoundTest pname protId =
  TestLabel (pname ++ " portAlreadyBoundTest") $ TestCase $ do
    (result :: Either SomeException ()) <- try $
      withTestServer serverOptions $ const $ do
        withEventBaseDataplane $ \evb ->
          withHeaderChannel evb headerConfig $
            lift $ withTestServer serverOptions $ const $ do
              withEventBaseDataplane $ \evb2 -> do
                withHeaderChannel evb2 headerConfig $
                  return ()
    assertBool "should fail" (isLeft result)
  where
    port :: Int
    port = 9999
    serverOptions :: ServerOptions
    serverOptions = ServerOptions
      { desiredPort = Just port
      , customFactoryFn = Nothing
      , customModifyFn = Nothing
      }
    headerConfig :: HeaderConfig t
    headerConfig = mkHeaderConfig port protId

tests :: String -> ProtocolId -> [Test]
tests pname protId = map (\f -> f pname protId)
  [ addTest
  , divideTest
  , divideExceptionTest
  , multiTest
  , unimplementedTest
  , echoTest
  , portAlreadyBoundTest
  ]

main :: IO ()
main = withFacebookUnitTest $
  testRunner $ TestList $
    tests "compact" compactProtocolId ++
    tests "binary" binaryProtocolId
