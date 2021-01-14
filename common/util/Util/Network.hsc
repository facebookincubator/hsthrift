-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ForeignFunctionInterface #-}

module Util.Network
  ( ntop6, ntop
  ) where

import Data.ByteString
import Data.Text
import Foreign (nullPtr)
import Foreign.C
import Foreign.Marshal.Alloc (allocaBytes)
import System.IO.Unsafe (unsafePerformIO)

import Util.Text (cStringToText)

ntop' :: Int -> Int -> ByteString -> Maybe Text
ntop' inet' inetAddrStrLen' source =
  unsafePerformIO $ useAsCString source $ \source' ->
    allocaBytes inetAddrStrLen' $ \target -> do
      result <- inet_ntop inet' source' target inet6AddrStrLen
      if result == nullPtr
        then return Nothing
        else Just <$> cStringToText result

ntop6 :: ByteString -> Maybe Text
ntop6 = ntop' inet6 inet6AddrStrLen

ntop :: ByteString -> Maybe Text
ntop = ntop' inet inetAddrStrLen

#include <arpa/inet.h>

inet6AddrStrLen :: Int
inet6AddrStrLen = #{ const INET6_ADDRSTRLEN }

inet6 :: Int
inet6 = #{ const AF_INET6 }

inetAddrStrLen :: Int
inetAddrStrLen = #{ const INET_ADDRSTRLEN }

inet :: Int
inet = #{ const AF_INET }

foreign import ccall unsafe "inet_ntop"
  inet_ntop :: Int -> CString -> CString -> Int -> IO CString
