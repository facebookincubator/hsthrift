-- Copyright (c) Facebook, Inc. and its affiliates.

module InstallFixtures (main) where

import Util

import Thrift.Compiler
import Thrift.Compiler.OptParse

main :: IO ()
main = withFixtureOptions $ mapM_ $ \(TheseOptions opts) -> run opts
