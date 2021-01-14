-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ImplicitParams #-}

-- |
-- This module allows you to log things to stderr. Make sure to initialize your
-- application using 'Facebook.Init.initFacebook' or
-- 'Facebook.Init.withFacebook' beforehand. Otherwise you're going to get a
-- cryptic "Logging before InitGoogleLogging() is written to STDERR" error.
module Util.Log.Text
  ( vlog
  , vlogIsOn
  , logInfo
  , logWarning
  , logError
  , logCritical
  ) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Foreign.C
import GHC.Stack

import Util.Log.Internal
import Util.Text (useTextAsCString)

-- | Equivalent to @VLOG(level)@, except that in Haskell we only evaluate
-- the message if verbosity at the desired level is enabled. Note that
-- this lazy evaluation of the message only applies to 'vlog', not to
-- any of the other logging functions.
vlog :: (MonadIO m, HasCallStack) => Int -> Text -> m ()
vlog level msg = liftIO $ do
  b <- vlogIsOn level
  when b $ logCommon c_glog_verbose (getCaller $ getCallStack callStack) msg

-- | The calling point is actually right next to the head as the head is the
-- current function (in this case it would be logInfo/Error/Critical).
getCaller :: [(String, SrcLoc)] -> (String, CInt)
getCaller cs =
  case cs of
    -- Take the first element of the stack if it exists
    ((_,sl):_) -> (srcLocFile sl, fromIntegral $ srcLocStartLine sl)
    _       -> ("Unknown stack trace", 0)

-- | Provided to support the new logging API. Please use @LOG_INFO()@
-- macro for this facility.
logInfo :: (MonadIO m, HasCallStack) => Text -> m ()
logInfo msg =
  liftIO $ logCommon c_glog_info (getCaller $ getCallStack callStack) msg

-- | Provided to support the new logging API. Please use @LOG_INFO()@
-- macro for this facility.
logWarning :: (MonadIO m, HasCallStack) => Text -> m ()
logWarning msg = liftIO $ logCommon c_glog_warning
  (getCaller $ getCallStack callStack) msg

-- | Provided to support the new logging API. Please use @LOG_ERROR()@
-- macro for this facility.
logError :: (MonadIO m, HasCallStack) => Text -> m ()
logError msg =
  liftIO $ logCommon c_glog_error (getCaller $ getCallStack callStack) msg

-- | Provided to support the new logging API. Please use
-- @LOG_CRITICAL()@ macro for this facility.
logCritical :: (MonadIO m, HasCallStack) => Text -> m ()
logCritical msg =
  liftIO $ logCommon c_glog_critical (getCaller $ getCallStack callStack) msg

logCommon :: (CString -> CInt -> CString -> IO ()) ->
             (String, CInt) -> Text -> IO ()
logCommon fn (file, lineNumber) msg =
  withCString file $ \file_cstring ->
    useTextAsCString msg $ \msg_cstring ->
      fn file_cstring lineNumber msg_cstring
