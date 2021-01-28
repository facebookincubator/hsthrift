-- Copyright (c) Facebook, Inc. and its affiliates.

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
