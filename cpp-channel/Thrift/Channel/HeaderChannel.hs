-- Copyright (c) Facebook, Inc. and its affiliates.

module Thrift.Channel.HeaderChannel
  ( HeaderWrappedChannel
  , HeaderConfig(..)
  , getRequestChannelProtocolId
  , withHeaderChannel
  , withHeaderChannelIO
  ) where

import Data.Proxy
import Foreign.C
import Foreign.Ptr

import Thrift.Channel.Lib.CppChannel
import Thrift.Channel.Lib.HeaderChannel
import Thrift.Monad
import Thrift.Protocol
import Util.EventBase

withHeaderChannel
    :: EventBaseDataplane
    -> HeaderConfig t
    -> (forall p . Protocol p => ThriftM p HeaderWrappedChannel t a)
    -> IO a
withHeaderChannel io HeaderConfig{..} fn = do
  withHeaderChannelForTransport io HeaderConfig{..} makeRawTransport fn

withHeaderChannelIO
    :: EventBaseDataplane
    -> HeaderConfig t
    -> (forall p . Protocol p => HeaderWrappedChannel t -> Proxy p -> IO a)
    -> IO a
withHeaderChannelIO io HeaderConfig{..} fn = do
  withHeaderChannelIOForTransport io HeaderConfig{..} makeRawTransport fn

foreign import ccall "&makeRawTransport"
  makeRawTransport ::
    FunPtr (CppSocketAddress -> EventBase -> CSize
      -> IO (Ptr CppAsyncTransport))
