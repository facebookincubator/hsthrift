{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.Err
  ( Err
  , errCtx
  , err
  ) where

-- | An error monad with multi-line nested error messages
type Err a = Either [String] a

err :: String -> Err a
err s = Left [s]

errCtx :: String -> Err a -> Err a
errCtx str e = case e of
  Left e' -> Left (str : map ("  " ++) e')
  Right a  -> Right a
