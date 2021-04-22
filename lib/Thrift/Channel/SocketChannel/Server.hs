-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
module Thrift.Channel.SocketChannel.Server
  ( -- * High level interface to socket server implementation
    withSocketServerOpts
  , SocketServerOptions(..)
  , defaultServerOptions
  , SocketServer(..)

  , -- * Lower level interface
    withServer
  , withServerIO
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Network.Socket
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Thrift.Api
import Thrift.Binary.Parser
import Thrift.Channel.SocketChannel
import Thrift.Monad
import Thrift.Processor
import Thrift.Protocol
import Thrift.Protocol.ApplicationException.Types
import Thrift.Protocol.Id

-- |
data SocketServerOptions = SocketServerOptions
  { desiredPort    :: Maybe Int
    -- ^ lets the OS pick a port when 'Nothing'
  , maxQueuedConns :: Maybe Int
    -- ^ hint for the max number of connections for 'listen',
    --   uses Network.Socket.maxListenQueue when Nothing
  , protocol       :: ProtocolId
    -- ^ defaults to the binary protocol
  } deriving (Eq, Show)

-- | Default options: let the OS pick a port, use 'Network.Socket.maxListenQueue'
--   as the maximum number of queued connections, and communicate
--   with the binary Thrift protocol.
defaultServerOptions :: SocketServerOptions
defaultServerOptions = SocketServerOptions Nothing Nothing binaryProtocolId

-- | Information about the running server given by 'withSocketServerOpts'
--   to the inner computation it runs against the server.
data SocketServer = SocketServer
  { serverPort :: Int
  , serverProt :: ProtocolId
  } deriving (Eq, Show)

-- | Highest level interface to the socket-based Thrift server
--   implementation.
withSocketServerOpts
  :: Processor s
  => (forall r. s r -> IO r) -- ^ server side handler
  -> SocketServerOptions
  -> (SocketServer -> IO ()) -- ^ computation to run while the server is up
  -> IO ()
withSocketServerOpts handler SocketServerOptions{..} action =
  withProxy protocol $ \protProxy ->
  withServerIO protProxy
               (fmap show desiredPort)
               (fromMaybe maxListenQueue maxQueuedConns)
               handler $
    \port -> action (SocketServer port protocol)

-- | Run a Thrift server and some client computations against
--   it, using the given handler to process requests.
withServer
  :: Processor s
  => ProtocolId               -- ^ protocol to use
  -> Maybe ServiceName        -- ^ desired port (if any)
  -> Int                      -- ^ max. number of queued connections
  -> (forall a . s a -> IO a) -- ^ server-side request handler
  -> Thrift t ()              -- ^ client computation
  -> IO ()
withServer protocol mport maxQueuedConns hndl action =
  withProxy protocol $ \proxy ->
    withServerIO proxy mport maxQueuedConns hndl $ \port ->
      withSocketChannel
        (SocketConfig localhost (fromIntegral port) protocol)
        action

-- | Bring up a server that will handle requests with the given handler,
--   and run client computations against it in 'IO'.
withServerIO
  :: forall c p. (Processor c, Protocol p)
  => Proxy p
  -> Maybe ServiceName       -- ^ desired port (if any)
  -> Int                     -- ^ max. number of queued connections
  -> (forall r. c r -> IO r) -- ^ server handler
  -> (Int -> IO ())          -- ^ client computation
  -> IO ()
withServerIO p mport maxQueuedConns handler client  = do
  counter <- newCounter
  flip (runServer mport maxQueuedConns)
       (\sock -> client . fromIntegral =<< socketPort sock) $
    \clientSock -> counter >>= \seqNum ->
      handleClient seqNum counter Nothing clientSock

  where handleClient seqNum counter mincompleteMsg sock =
          handleProtocolException p sock seqNum $
            handleClient' seqNum counter mincompleteMsg sock
        handleClient' seqNum counter mincompleteMsg sock = do
          minput <- try (threadWaitRecv sock recvBlockBytes)
          case minput of
            Left (e :: SomeException) -> do
              throwProtocolException $ unwords
                [ "an exception was raised while trying to read from"
                , "the client socket: " ++ show e
                ]
            Right input
              | BS.null input -> return ()
              | otherwise -> do
                  let input' = maybe input (<> input) mincompleteMsg
                  (mincomplete', seqNum') <-
                    processInput seqNum counter input' sock
                  handleClient seqNum' counter mincomplete' sock

        processInput seqNum counter input sock =
          -- TODO: we'd perhaps like to know how much progress we make, to
          --       distinguish bad input that we'll never be able to make sense
          --       of, no matter how many more bytes we add to it, and
          --       large commands that we receive in several chunks.
          case parseAndLeftover (msgParser p) input of
            -- 'input' is an incomplete command, we keep it around
            -- to add more bytes to it before attempting to parse and
            -- process it again
            Left _ -> return (Just input, seqNum)

            -- 'input' contains at least a complete command, that we
            -- process. If there's some leftover, we try and extract
            -- a command from it as well, potentially doing this several
            -- times, until either we're done or more input is needed to go
            -- further.
            Right (Some cmd, leftover) -> do
              (response, mexc) <- processCommand p seqNum handler cmd
              seqNum' <- counter
              case mexc of
                Just (_exc, _blame) -> do
                  _ <- sendBS sock (toStrict $ toLazyByteString response)
                  return (Nothing, seqNum')
                Nothing -> do
                  unless (reqName cmd `elem` onewayFns (Proxy :: Proxy c)) $
                    void $ sendBS sock (toStrict $ toLazyByteString response)
                  if BS.null leftover
                    then return (Nothing, seqNum')
                    else processInput seqNum' counter leftover sock

sendException
  :: Protocol p => Proxy p -> Socket -> SeqNum -> ApplicationException -> IO ()
sendException proxy sock seqNum exc =
  void $ sendBS sock protocolExcMsg
  where protocolExcMsg = toStrict . toLazyByteString $
          genMsgBegin proxy "" 3 seqNum <>
          buildStruct proxy exc <>
          genMsgEnd proxy

handleProtocolException
  :: Protocol p
  => Proxy p
  -> Socket
  -> SeqNum
  -> IO ()
  -> IO ()
handleProtocolException proxy sock seqNum m = m `catch` hndl
  where hndl :: ApplicationException -> IO ()
        hndl exc = sendException proxy sock seqNum exc

throwProtocolException :: String -> IO a
throwProtocolException err = throwIO (mkProtocolException err)

mkProtocolException :: String -> ApplicationException
mkProtocolException err = ApplicationException (T.pack err)
  ApplicationExceptionType_ProtocolError

runServer
  :: Maybe ServiceName -- ^ desired port (if any)
  -> Int               -- ^ max. number of queued connections
  -> (Socket -> IO ()) -- ^ server computation
  -> (Socket -> IO ())  -- ^ client computation
  -> IO ()
runServer mport maxQueuedConns server client = do
  withSocketServer mport maxQueuedConns
    (\servSock _sa -> client servSock)
    (\sock _sa -> server sock)

withSocketServer
  :: Maybe ServiceName             -- desired port (Nothing to bind any available port)
  -> Int                           -- maximum number of queued connections
  -> (Socket -> SockAddr -> IO ()) -- what to do when the server is up
  -> (Socket -> SockAddr -> IO ()) -- what to do on a new client connection
  -> IO ()
withSocketServer mport maxQueuedConns onServerUp onNewClient = withSocketsDo $ do
  addr <- resolveServer localhost mport
  bracket (setupServerSock maxQueuedConns addr) teardownSock $ \sock ->
    withAsync (acceptLoop sock) $ \_async ->
      onServerUp sock (addrAddress addr)

  where acceptLoop sock = forever $ do
          (clientSock, clientAddr) <- accept sock
          forkFinally (onNewClient clientSock clientAddr)
                      (\_res -> mask_ $ teardownSock clientSock)

setupServerSock :: Int -> AddrInfo -> IO Socket
setupServerSock maxQueuedConns addr = do
  s <- openSocket addr
  bind s (addrAddress addr)
  listen s maxQueuedConns
  return s

resolveServer :: HostName -> Maybe ServiceName -> IO AddrInfo
resolveServer host mport = do
  results <- getAddrInfo (Just hints) (Just host) mport
  case results of
    [] -> error "SocketServer.resolveServer: getAddrInfo returned an empty list"
    (a:_) -> return a

  where hints = defaultHints { addrSocketType = Stream }

#if !MIN_VERSION_network(3,1,2)
openSocket :: AddrInfo -> IO Socket
openSocket addr =
  socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
#endif
