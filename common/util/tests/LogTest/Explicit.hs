-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE NoOverloadedStrings #-}

module LogTest.Explicit
  ( run
  ) where

import qualified Data.Text as Text

import qualified Util.Log.String as LS
import qualified Util.Log.Text as LT


run :: IO ()
run = do
  -- regular / String messages
  LS.vlog 3 "Log VERBOSE example"
  LS.logInfo "Log INFO example"
  LS.logWarning "Log WARNING example"
  LS.logError "Log ERROR example"

  -- Text messages
  LT.vlog 3 $ Text.pack "Log VERBOSE w/ Text example"
  LT.logInfo $ Text.pack "Log INFO w/ Text example"
  LT.logWarning $ Text.pack "Log WARNING w/ Text example"
  LT.logError $ Text.pack "Log ERROR w/ Text example"
