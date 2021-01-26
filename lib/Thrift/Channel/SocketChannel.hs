-- Copyright (c) Facebook, Inc. and its affiliates.

-- |
-- An implementation of a Thrift channel in Haskell using
-- 'Network.Socket'. Note that the wire format of messages is not
-- necessarily compatible with other transports, in particular you
-- can't mix and match SocketChannel clients/servers with
-- HeaderChannel clients/servers.

{-# LANGUAGE CPP #-}
module Thrift.Channel.SocketChannel
  ( -- * Socket channel types and API
    SocketChannel(..)
  , SocketConfig(..)
  , withSocketChannel
  , withSocketChannelIO

  , -- * Utilities used by test servers
    sendBS
  , teardownSock
  , threadWaitRecv
  , recvBlockBytes
  , localhost
  ) where

import Control.Concurrent
import Control.Exception
import Data.Proxy
import Network.Socket
import Network.Socket.ByteString
import System.Posix.Types (Fd(..))
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Thrift.Channel
import Thrift.Monad
import Thrift.Protocol
import Thrift.Protocol.Id

newtype SocketChannel t = SocketChannel Socket

instance ClientChannel SocketChannel where
  sendRequest ch@(SocketChannel sock) req sendcb recvcb = do
    sendOnewayRequest ch req sendcb
    recvRes <- try (threadWaitRecv sock recvBlockBytes)
    case recvRes of
      Right s -> recvcb (Right $ Response s mempty)
      Left (e :: SomeException) ->
        recvcb (Left $ ChannelException $ T.pack (show e))

  sendOnewayRequest (SocketChannel sock) req sendcb = do
    res <- sendBS sock (reqMsg req)
    sendcb res

data SocketConfig a = SocketConfig
  { socketHost       :: HostName
  , socketPortNum    :: PortNumber
  , socketProtocolId :: ProtocolId
  }

deriving instance Show (SocketConfig a)

-- | Given a 'SocketConfig' that specifies where to find the
--   server we want to reach and what protocol it uses,
--   connect to the server and run the given client
--   computation with it.
withSocketChannel
  :: SocketConfig t
  -> (forall p . Protocol p => ThriftM p SocketChannel t a)
  -> IO a
withSocketChannel SocketConfig{..} f =
  withSocketChannelIO socketHost socketPortNum $ \ch -> do
    withProxy socketProtocolId (go ch f)

  where go :: Protocol p
           => SocketChannel t
           -> ThriftM p SocketChannel t a
           -> Proxy p
           -> IO a
        go ch f _proxy = runThrift f ch

-- | A lower level variant of 'withSocketChannel' that connects
--   to the server at the given host and port and passes the
--   resulting 'SocketChannel' to the third argument.
withSocketChannelIO
  :: HostName
  -> PortNumber
  -> (SocketChannel t -> IO a)
  -> IO a
withSocketChannelIO host port f = withSocketClient host port $ \sock _ ->
  f (SocketChannel sock)

sendBS :: Socket -> BS.ByteString -> IO (Maybe ChannelException)
sendBS sock bs = try (sendAll sock bs) >>= \res -> case res of
  Left  e -> return (Just e)
  Right _ -> return Nothing

-- | Block (using 'threadWaitRead') until some data is available,
--   and then call 'recv' to grab it.
threadWaitRecv
  :: Socket
  -> Int -- ^ max. number of bytes we want to receive
  -> IO BS.ByteString
threadWaitRecv sock numBytes = withFdSocket sock $ \fd ->
  threadWaitRead (Fd fd) >> recv sock numBytes

withSocketClient
  :: HostName
  -> PortNumber
  -> (Socket -> AddrInfo -> IO a)
  -> IO a
withSocketClient host port f = withSocketsDo $ do
  addr <- resolveClient host port
  bracket (setupClientSock addr) teardownSock $ \sock ->
    f sock addr

setupClientSock :: AddrInfo -> IO Socket
setupClientSock addr = do
  s <- openSocket addr
  connect s (addrAddress addr)
  return s

teardownSock :: Socket -> IO ()
teardownSock sock = shutdown sock ShutdownBoth `finally` close sock

resolveClient :: HostName -> PortNumber -> IO AddrInfo
resolveClient host port = do
  results <- getAddrInfo (Just hints) (Just host) (Just $ show port)
  case results of
    [] -> error $ "SocketChannel.resolveClient: " <>
      "getAddrInfo returned an empty list"
    (a:_) -> return a

  where hints = defaultHints { addrSocketType = Stream }

localhost :: HostName
localhost =
#ifdef IPV4
  "127.0.0.1"
#else
  "::1"
#endif

recvBlockBytes :: Int
recvBlockBytes = 4096

#if !MIN_VERSION_network(3,1,2)
openSocket :: AddrInfo -> IO Socket
openSocket addr =
  socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
#endif
