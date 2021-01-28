-- Copyright (c) Facebook, Inc. and its affiliates.

module Main (main) where

import Control.Monad
import Options.Applicative

import Thrift.Compiler as Compiler
import Thrift.Compiler.OptParse

main :: IO ()
main = do
  TheseOptions opts <- execParser (info (helper <*> optionsParser) fullDesc)
  void $ Compiler.run opts
