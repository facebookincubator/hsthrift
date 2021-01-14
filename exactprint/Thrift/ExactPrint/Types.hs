-- Copyright (c) Facebook, Inc. and its affiliates.

module Thrift.ExactPrint.Types
  ( Offset(..)
  ) where

data Offset = Offset
  { offsRows :: !Int -- ^ Vertical Offset
  , offsCols :: !Int -- ^ Horizontal Offset (from beginning of the line if
                     --   offsRows > 0)
  } deriving (Show, Eq)
