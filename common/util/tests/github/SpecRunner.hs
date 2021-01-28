-- Copyright (c) Facebook, Inc. and its affiliates.

module SpecRunner where

import Test.Hspec

specRunner :: Spec -> IO ()
specRunner = hspec
