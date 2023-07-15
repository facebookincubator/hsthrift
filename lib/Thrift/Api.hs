{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Thrift.Api
  ( Thrift
  ) where

import Thrift.Channel
import Thrift.Monad
import Thrift.Protocol

-- | A thrift call across any protocol or channel for service `s` that returns
-- an`a`
type Thrift s a =
  forall p c . (Protocol p, ClientChannel c) => ThriftM p c s a
