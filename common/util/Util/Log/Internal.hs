-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TemplateHaskell #-}

module Util.Log.Internal
  ( vlogIsOn
  , c_glog_verbose
  , c_glog_info
  , c_glog_warning
  , c_glog_error
  , c_glog_critical
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
  "void glog_critical(const char*, int, const char*)"
  [d|
    foreign import ccall unsafe
      c_glog_critical :: CString -> CInt -> CString -> IO ()
  |])

{-# INLINE vlogIsOn #-}
vlogIsOn :: MonadIO m => Int -> m Bool
vlogIsOn level = liftIO $ do
  x <- c_vlog_is_on (fromIntegral level)
  return (x /= 0)
