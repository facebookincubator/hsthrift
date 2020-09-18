module SpecRunner where

import Test.Hspec

specRunner :: Spec -> IO ()
specRunner = hspec
