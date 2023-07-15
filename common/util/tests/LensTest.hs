{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TemplateHaskell #-}

module LensTest where

import Control.Lens
import Test.HUnit
import TestRunner
import Util.Lens

data Foo = Foo
  { foo_bar :: Int
  }

defFoo :: Foo
defFoo = Foo { foo_bar = 42 }

makeLowerCCLenses ''Foo

readTest :: Test
readTest = TestLabel "read test" $ TestCase $
  assertEqual "foo bar baz" 43 (foo_bar $ adjust defFoo)
  where
    adjust = over fooBar (+ 1)

main :: IO ()
main = testRunner $ TestList
  [ readTest
  ]
