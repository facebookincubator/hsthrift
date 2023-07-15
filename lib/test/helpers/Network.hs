{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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
