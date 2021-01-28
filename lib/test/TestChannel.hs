-- Copyright (c) Facebook, Inc. and its affiliates.

module TestChannel
  ( TestChannel(..), Req(..)
  , runServer
  , runTestServer
  ) where

import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import Data.Proxy

import Thrift.Channel
import Thrift.Monad (getRpcPriority, newCounter)
import Thrift.Processor
import Thrift.Protocol

newtype TestChannel s = TestChannel (MVar Req)

data Req = Req ByteString RecvCallback

instance ClientChannel TestChannel where
  sendRequest (TestChannel reqBuf) Request{..} sendCob recvCob =
    case getRpcPriority reqOptions of
      Nothing             -> send ()
      Just NormalPriority -> send ()
      _ -> sendCob $ Just $ ChannelException "non-Normal priority"
    where
      send () = sendCob Nothing >> putMVar reqBuf (Req reqMsg recvCob)

  sendOnewayRequest (TestChannel reqBuf) Request{..} sendCob = do
    putMVar reqBuf $ Req reqMsg (\_ -> return ())
    sendCob Nothing

runServer
  :: (Processor c, Protocol p)
  => Proxy p
  -> TestChannel s
  -> (forall r . c r -> IO r)
  -> IO ()
runServer p ch handler = do
  counter <- newCounter
  runTestServer ch $ \bytes -> do
    seqNum <- counter
    process p seqNum handler bytes

runTestServer
  :: TestChannel s
  -> (ByteString -> IO (ByteString, a))
  -> IO ()
runTestServer (TestChannel req) handler = forever $ do
  Req bytes callback <- takeMVar req
  (handled, _) <- handler bytes
  callback $ Right $ Response handled mempty
