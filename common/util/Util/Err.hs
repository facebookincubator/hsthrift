-- Copyright (c) 2014, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

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
