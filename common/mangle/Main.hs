-- Copyright (c) Facebook, Inc. and its affiliates.

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
