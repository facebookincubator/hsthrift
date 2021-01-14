-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoOverloadedStrings #-}

module LogTest.MonadStack
  ( run
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Reader
import qualified Data.Text as Text

import qualified Util.Log as L
import qualified Util.Log.Text as LT


run :: IO ()
run = execTestM monadStack


newtype TestM a = TestM (ReaderT () IO a)
                  deriving (Functor, Applicative, Monad,
                            MonadReader (), MonadIO)

execTestM :: TestM a -> IO a
execTestM (TestM inner) = runReaderT inner ()

monadStack :: TestM ()
monadStack = do
  -- regular / String messages
  L.logInfo "Log INFO in a monad stack example"
  L.logError "Log ERROR in a monad stack example"
  L.logCritical "Log CRITICAL in a monad stack example"

  -- Text messages
  LT.logInfo $ Text.pack "Log INFO w/ Text in a monad stack example"
  LT.logError $ Text.pack "Log ERROR w/ Text in a monad stack example"
  LT.logCritical $ Text.pack "Log CRITICAL w/ Text in a monad stack example"
