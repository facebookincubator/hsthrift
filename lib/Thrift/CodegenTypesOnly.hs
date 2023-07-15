{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Thrift.CodegenTypesOnly
  ( module Thrift
  ) where

import Thrift.Protocol as Thrift
import Thrift.Protocol.JSON as Thrift
import Thrift.Protocol.JSON.Base64 as Thrift

import Thrift.HasFields as Thrift

import Data.ByteString(ByteString)
import qualified Data.ByteString as ByteString
import Data.Default

instance Default ByteString where def = ByteString.empty
