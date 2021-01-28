-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE OverloadedStrings #-}

module LogTest.OverloadedStrings
  ( run
  ) where

import qualified Util.Log as L
import qualified Util.Log.Text as LT


run :: IO ()
run = do
  L.logInfo "Log INFO w/ OverloadedStrings example"
  L.logError "Log ERROR w/ OverloadedStrings example"
  L.logCritical "Log CRITICAL w/ OverloadedStrings example"

  LT.logInfo "Log.Text INFO w/ OverloadedStrings example"
  LT.logError "Log.Text ERROR w/ OverloadedStrings example"
  LT.logCritical "Log.Text CRITICAL w/ OverloadedStrings example"
