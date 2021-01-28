-- Copyright (c) Facebook, Inc. and its affiliates.

module DanglingPointerTest (main) where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Text (Text)

import Facebook.Init
import Network (testServerHost)
import Test.HUnit
import TestRunner

import Thrift.Channel.HeaderChannel
import Thrift.Protocol.Id
import Util.EventBase

import HashMap.HashMapService.Client
import HashMap.HashMapService.Service
import HashMap.Types

import Thrift.Server.CppServer

-- Handler ---------------------------------------------------------------------

handler
  :: IORef (HashMap Text ByteString)
  -> HashMapServiceCommand a
  -> IO a
handler state (Put key val) = modifyIORef' state $ HashMap.insert key val
handler state (Get key) = do
  m <- readIORef state
  case HashMap.lookup key m of
    Just val -> return val
    Nothing -> throwIO $ NotFound key

-- Run Server ------------------------------------------------------------------

runServer
  :: EventBaseDataplane
  -> IORef (HashMap Text ByteString)
  -> [Text]
  -> ProtocolId
  -> IO ()
runServer evb st keys protId =
  withBackgroundServer (handler st) defaultOptions $ \Server{..} -> do
    -- Run the client, populate the HashMap with some values
    let
      conf :: HeaderConfig HashMapService
      conf = HeaderConfig
        { headerHost = testServerHost
        , headerPort = serverPort
        , headerProtocolId = protId
        , headerConnTimeout = 5000
        , headerSendTimeout = 5000
        , headerRecvTimeout = 5000
        }
    withHeaderChannel evb conf $ forM_ keys $ \key ->
      put key "XXX"

testValue :: IORef (HashMap Text ByteString) -> Text -> Assertion
testValue st key = do
  m <- readIORef st
  case HashMap.lookup key m of
    Nothing -> assertFailure $ "Key '" ++ show key ++ "' is missing"
    Just val -> assertEqual "Value is XXX" "XXX" val

-- Main ------------------------------------------------------------------------

main :: IO ()
main = do
  ref <- newIORef HashMap.empty
  withFacebookUnitTest $ withEventBaseDataplane $ \evb -> do
    sequence_
      [ runServer evb ref ["W", "X"] compactProtocolId
      , runServer evb ref ["Y", "Z"] binaryProtocolId
      ]

    testRunner $ TestLabel "Dangling Pointer" $ TestCase $
      forM_ ["W", "X", "Y", "Z"] $ testValue ref
