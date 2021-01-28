-- Copyright (c) Facebook, Inc. and its affiliates.

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
