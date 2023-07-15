{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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
