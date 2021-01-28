-- Copyright (c) Facebook, Inc. and its affiliates.

module LogTest (main) where

import Facebook.Init

import qualified LogTest.Explicit as Explicit
import qualified LogTest.MonadStack as MonadStack
import qualified LogTest.OverloadedStrings as OverloadedStrings


main :: IO ()
main = withFacebook $ do
  Explicit.run
  OverloadedStrings.run
  MonadStack.run
