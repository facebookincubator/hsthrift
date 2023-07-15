{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Thrift.ExactPrint.Types
  ( Offset(..)
  ) where

data Offset = Offset
  { offsRows :: !Int -- ^ Vertical Offset
  , offsCols :: !Int -- ^ Horizontal Offset (from beginning of the line if
                     --   offsRows > 0)
  } deriving (Show, Eq)
