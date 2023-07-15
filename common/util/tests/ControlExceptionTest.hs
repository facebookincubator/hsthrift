{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module ControlExceptionTest where

import Control.Exception
import Util.Control.Exception

import Test.HUnit
import TestRunner

import Facebook.Init (withFacebookUnitTest)

newtype MyException = MyException String deriving (Eq, Show)
instance Exception MyException

main :: IO ()
main = withFacebookUnitTest $ testRunner $ TestLabel "throwLeftIO" $ TestList
  [ TestLabel "with left" $ TestCase $ do
    let
      ex = MyException "bad!"
    (res :: Either MyException ()) <- try $ throwLeftIO $ Left ex
    assertEqual "throws the exception" res (Left ex)
  , TestLabel "with right" $ TestCase $ do
    let
      val = "good"
    (res :: Either MyException String) <- try $ throwLeftIO
      (Right val :: Either MyException String)
    assertEqual "returns the value" res (Right val)
  ]
