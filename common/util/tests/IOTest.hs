-- Copyright (c) Facebook, Inc. and its affiliates.

module IOTest
  ( main
  ) where

import System.Directory
import System.FilePath
import System.IO.Temp
import TestRunner
import Test.HUnit
import Util.IO

main :: IO ()
main = testRunner $ TestList
  [ writeFileAtomicUTF8Test ]

writeFileAtomicUTF8Test :: Test
writeFileAtomicUTF8Test = TestLabel
  "creates if file and parent dir does not exist" $ TestCase $
    withSystemTempDirectory "writeFileAtomicUTF8Test" $ \tmpRoot -> do
      let
        testFile = tmpRoot </> "nested" </> "writeFileAtomicUTF8Test.tmp"
      do
        writeFileAtomicUTF8 testFile "writeFileAtomicUTF8Test"
        doesFileExist testFile >>= assertBool "File should exist"
