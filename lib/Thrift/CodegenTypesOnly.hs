-- Copyright (c) Facebook, Inc. and its affiliates.

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
