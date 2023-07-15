{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Main (main) where

import System.Environment
import System.Exit hiding (die)
import System.IO

import Mangle

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> either (die 1 . show) putStr $ mangle arg
    _ -> die 1 "Usage: mangle <signature>"

die :: Int -> String -> IO a
die exitCode msg =
  hPutStrLn stderr msg >> exitWith (ExitFailure exitCode)
