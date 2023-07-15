{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Thrift.Protocol.Id
  ( withProxy
  , ProtocolId
  , binaryProtocolId
  , compactProtocolId
  ) where

import Control.Exception
import Data.Proxy
import Foreign.C
import Thrift.Protocol
import Thrift.Protocol.Binary
import Thrift.Protocol.Compact

type ProtocolId = CUShort

binaryProtocolId :: ProtocolId
binaryProtocolId = 0

compactProtocolId :: ProtocolId
compactProtocolId = 2

withProxy :: ProtocolId -> (forall p . Protocol p => Proxy p -> IO a) -> IO a
withProxy i action
  | i == binaryProtocolId = action (Proxy :: Proxy Binary)
  | i == compactProtocolId = action (Proxy :: Proxy Compact)
  | otherwise = throw $ ProtocolException $ "unknown protocol id: " ++ show i
