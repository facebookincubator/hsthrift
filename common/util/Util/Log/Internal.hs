{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TemplateHaskell #-}

module Util.Log.Internal
  ( vlogIsOn
  , c_glog_verbose
  , c_glog_info
  , c_glog_warning
  , c_glog_error
  , c_glog_fatal
  , c_glog_flush
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C (CString, CInt(..))

import Mangle.TH

$(mangle
  "int vlog_is_on(int)"
  [d|
    foreign import ccall unsafe
      c_vlog_is_on :: CInt -> IO CInt
  |])

$(mangle
  "void glog_verbose(const char*, int, const char*)"
  [d|
    foreign import ccall unsafe
      c_glog_verbose :: CString -> CInt -> CString -> IO ()
  |])

$(mangle
  "void glog_info(const char*, int, const char*)"
  [d|
    foreign import ccall unsafe
      c_glog_info :: CString -> CInt -> CString -> IO ()
  |])

$(mangle
  "void glog_warning(const char*, int, const char*)"
  [d|
    foreign import ccall unsafe
      c_glog_warning :: CString -> CInt -> CString -> IO ()
  |])

$(mangle
  "void glog_error(const char*, int, const char*)"
  [d|
    foreign import ccall unsafe
      c_glog_error :: CString -> CInt -> CString -> IO ()
  |])

$(mangle
  "void glog_fatal(const char*, int, const char*)"
  [d|
    foreign import ccall unsafe
      c_glog_fatal :: CString -> CInt -> CString -> IO ()
  |])

$(mangle
  "void glog_flush()"
  [d|
    foreign import ccall unsafe
      c_glog_flush :: IO ()
  |])

{-# INLINE vlogIsOn #-}
vlogIsOn :: MonadIO m => Int -> m Bool
vlogIsOn level = liftIO $ do
  x <- c_vlog_is_on (fromIntegral level)
  return (x /= 0)
