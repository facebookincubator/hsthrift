-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Log
  ( vlog
  , logInfo
  , logWarning
  , logError
  , logCritical
  ) where

-- Re-export all String-based logging functions for compatibility
-- with existing code.
import Util.Log.String
