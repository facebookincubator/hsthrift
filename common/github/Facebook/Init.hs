{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Facebook.Init where

withFacebook :: IO () -> IO ()
withFacebook = id

withFacebookUnitTest :: IO a -> IO a
withFacebookUnitTest = id
