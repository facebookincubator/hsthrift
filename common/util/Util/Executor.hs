-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Executor
  ( CppFollyExecutor
  , Executor(..)
  , ExecutorKeepAlive(..)
  , ExecutorProvider(..)
  , getGlobalCPUExecutor
  , releaseGlobalCPUExecutor
  , withGlobalCPUExecutor
  ) where

import Foreign.Ptr
import Control.Exception (bracket)

-- CppExecutor is a folly::Executor*
data CppFollyExecutor
newtype Executor = Executor (Ptr CppFollyExecutor)

data CppFollyExecutorKeepAlive
newtype ExecutorKeepAlive = ExecutorKeepAlive (Ptr CppFollyExecutorKeepAlive)

class ExecutorProvider a where
  getExecutor :: a -> IO Executor

instance ExecutorProvider Executor where
  getExecutor = return

instance ExecutorProvider ExecutorKeepAlive where
  getExecutor (ExecutorKeepAlive p) = Executor <$> c_getExecutorFromKeepAlive p

getGlobalCPUExecutor :: IO ExecutorKeepAlive
getGlobalCPUExecutor = ExecutorKeepAlive <$> c_getGlobalCPUExecutor

releaseGlobalCPUExecutor :: ExecutorKeepAlive -> IO ()
releaseGlobalCPUExecutor (ExecutorKeepAlive p) = c_releaseGlobalCPUExecutor p

withGlobalCPUExecutor
    :: (ExecutorKeepAlive -> IO b)
    -> IO b
withGlobalCPUExecutor = bracket getGlobalCPUExecutor releaseGlobalCPUExecutor

foreign import ccall unsafe "common_hs_getGlobalCPUExecutor"
  c_getGlobalCPUExecutor :: IO (Ptr CppFollyExecutorKeepAlive)

foreign import ccall unsafe "common_hs_releaseGlobalCPUExecutor"
  c_releaseGlobalCPUExecutor :: Ptr CppFollyExecutorKeepAlive -> IO ()

foreign import ccall unsafe "common_hs_getExecutorFromKeepAlive"
  c_getExecutorFromKeepAlive
    :: Ptr CppFollyExecutorKeepAlive -> IO (Ptr CppFollyExecutor)
