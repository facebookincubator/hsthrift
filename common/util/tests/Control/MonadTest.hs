{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Control.MonadTest (main) where

import Test.HUnit
import TestRunner

import Facebook.Init
import Util.Control.Monad

firstMLazyTest :: Test
firstMLazyTest = TestLabel "firstMLazy" . TestCase $ do
  r <- firstMLazy []
  assertEqual "Nothing" Nothing (r :: Maybe Int)

  r <- firstMLazy
    [ return Nothing
    , return $ Just 1
    , return $ Just 2
    , error "notFound"
    ]
  assertEqual "Just" (Just 1) (r :: Maybe Int)

main :: IO ()
main = withFacebookUnitTest $
  testRunner firstMLazyTest
