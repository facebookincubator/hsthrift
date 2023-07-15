{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module InstallFixtures (main) where

import Util

import Thrift.Compiler
import Thrift.Compiler.OptParse

main :: IO ()
main = withFixtureOptions $ mapM_ $ \(TheseOptions opts) -> run opts
