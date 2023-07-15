{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}

module LogTest.OverloadedStrings
  ( run
  ) where

import qualified Util.Log as L
import qualified Util.Log.Text as LT


run :: IO ()
run = do
  L.logInfo "Log INFO w/ OverloadedStrings example"
  L.logWarning "Log WARNING w/ OverloadedStrings example"
  L.logError "Log ERROR w/ OverloadedStrings example"

  LT.logInfo "Log.Text INFO w/ OverloadedStrings example"
  LT.logWarning "Log.Text WARNING w/ OverloadedStrings example"
  LT.logError "Log.Text ERROR w/ OverloadedStrings example"
