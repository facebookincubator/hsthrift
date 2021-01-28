-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
module Network where

import Data.ByteString.Char8 (ByteString, pack)

testServerHost :: ByteString
testServerHost = pack
#ifdef IPV4
  "127.0.0.1"
#else
  "::1"
#endif
