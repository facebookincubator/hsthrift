-- Copyright (c) Facebook, Inc. and its affiliates.

module Thrift.Channel
  ( ClientChannel(..)
  , SendCallback, RecvCallback
  , Handle, mkCallbacks, wait
  , Request(..), RpcOptions(..)
  , Priority(..), Header
  , defaultRpcOptions, simpleRequest
  , Response(..), ChannelException(..)
  ) where

import Control.Exception hiding (handle)
import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import Data.Text (Text)

import Thrift.Monad

-- Channels --------------------------------------------------------------------

class ClientChannel (c :: * -> *) where
  sendRequest       :: c t -> Request -> SendCallback -> RecvCallback -> IO ()
  sendOnewayRequest :: c t -> Request -> SendCallback -> IO ()

type SendCallback = Maybe ChannelException -> IO ()
type RecvCallback = Either ChannelException Response -> IO ()

-- Default Callback Implementation with an MVar --------------------------------

type Handle a = MVar (Either SomeException a)

mkCallbacks
  :: (Response -> Either SomeException a)
  -> IO (Handle a, SendCallback, RecvCallback)
mkCallbacks deserialize = do
  handle <- newEmptyMVar
  let
    sendCob Nothing = return ()
    -- If there is a send error, then recv will never get called
    sendCob (Just err) = putMVar handle (Left $ SomeException err)

    recvCob (Left err) = putMVar handle (Left $ SomeException err)
    recvCob (Right r)  = putMVar handle $ deserialize r
  return (handle, sendCob, recvCob)

wait :: Handle a -> IO a
wait = takeMVar >=> either throw return

-- Requests --------------------------------------------------------------------

data Request = Request
  { reqMsg     :: !ByteString
  , reqOptions :: !RpcOptions
  }

simpleRequest :: ByteString -> Request
simpleRequest bs = Request { reqMsg     = bs
                           , reqOptions = defaultRpcOptions
                           }

-- Responses -------------------------------------------------------------------

data Response = Response
  { respMsg    :: ByteString
  , respHeader :: Header
  }

type Header = [(ByteString, ByteString)]

newtype ChannelException = ChannelException Text
                         deriving (Show, Eq)
instance Exception ChannelException
