{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Main (main) where

import Control.Monad
import Options.Applicative

import Thrift.Compiler as Compiler
import Thrift.Compiler.OptParse

main :: IO ()
main = do
  TheseOptions opts <- execParser (info (helper <*> optionsParser) fullDesc)
  void $ Compiler.run opts
