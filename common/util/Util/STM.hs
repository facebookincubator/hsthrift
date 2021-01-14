-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.STM
 ( updateTVar
 ) where

import Control.Concurrent.STM

-- | version of @modifyTVar'@ that returns the new value
updateTVar :: TVar a -> (a -> a) -> STM a
updateTVar var f = do
  x <- readTVar var
  let !x' = f x
  writeTVar var x'
  return x'
