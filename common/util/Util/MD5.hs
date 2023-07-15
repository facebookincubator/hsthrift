{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.MD5 (md5) where

import GHC.Fingerprint
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import Data.Text
import System.IO.Unsafe
import Foreign

import Util.Text

md5 :: ByteString -> Text
md5 bs =
  unsafeDupablePerformIO $
    unsafeUseAsCStringLen bs $ \(ptr,len) ->
      textShow <$> fingerprintData (castPtr ptr) (fromIntegral len)
