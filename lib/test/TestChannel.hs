module TestChannel
  ( TestChannel(..), Req(..)
  , runTestServer
  ) where

import Control.Exception
import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)

import Thrift.Channel
import Thrift.Monad (getRpcPriority)

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
    putMVar reqBuf $ Req reqMsg
      (throw (ChannelException "no readbuf for oneway request"))
    sendCob Nothing

runTestServer
  :: TestChannel s
  -> (ByteString -> IO (ByteString, a))
  -> IO ()
runTestServer (TestChannel req) handler = forever $ do
  Req bytes callback <- takeMVar req
  (handled, _) <- handler bytes
  callback $ Right $ Response handled mempty
