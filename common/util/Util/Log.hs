{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.Log
  ( vlog
  , logInfo
  , logWarning
  , logError
  , logFatal
  , flush
  ) where

import Control.Monad.IO.Class
-- Re-export all String-based logging functions for compatibility
-- with existing code.
import Util.Log.String
import Util.Log.Internal

flush :: MonadIO m => m ()
flush = liftIO c_glog_flush
