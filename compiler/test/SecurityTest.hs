{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SecurityTest (main) where

import Control.Exception (try)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.HUnit
import TestRunner

import Thrift.Compiler
import Thrift.Compiler.GenHaskell
  ( NamespacePathValidationError(..)
  , genHsCode
  )
import Thrift.Compiler.Options
import Thrift.Compiler.Plugins.Haskell

-- | Main test runner
main :: IO ()
main = testRunner $ TestList
  [ testAbsolutePathNamespaceRejected
  , testRelativePathTraversalRejected
  ]

-- | Test that absolute path namespaces do not escape output directory
testAbsolutePathNamespaceRejected :: Test
testAbsolutePathNamespaceRejected =
  TestLabel "Absolute Path Namespace Security" $ TestCase $
  withSystemTempDirectory "thrift-namespace-test" $ \tmpDir -> do
    let outputPath = tmpDir </> "output"
        escapingPath = tmpDir </> "other_location"
        inputFile = tmpDir </> "absolute_path_test.thrift"

    -- Generate thrift file with absolute path namespace
    let thriftContent = unlines
          [ "namespace hs \"" ++ escapingPath ++ "\""
          , "struct TestStruct { 1: i32 value }"
          ]
    writeFile inputFile thriftContent

    -- Create output directory
    createDirectoryIfMissing True outputPath

    -- Compile the thrift file - should throw NamespacePathValidationError
    let opts = (defaultOptions defaultHsOpts)
          { optsPath = inputFile
          , optsOutPath = outputPath
          , optsRecursive = True
          , optsGenMode = EmitCode
          }
    result <- try @NamespacePathValidationError $ do
      (headModule, _deps) <- typecheckInput opts =<< parseAll opts inputFile
      genHsCode opts headModule

    case result of
      Left NamespacePathValidationError{} ->
        return ()  -- ✓ Expected validation error

      Right _modules ->
        assertFailure
          "Compilation succeeded but should have failed validation"

-- | Test that relative path traversal is rejected
testRelativePathTraversalRejected :: Test
testRelativePathTraversalRejected =
  TestLabel "Relative Path Traversal Security" $ TestCase $
  withSystemTempDirectory "thrift-namespace-test-rel" $ \tmpDir -> do
    let outputPath = tmpDir </> "nested" </> "output"
        traversal = ".." </> ".." </> ".." </> ".." </> "other_location"
        inputFile = tmpDir </> "relative_path_test.thrift"

    -- Generate thrift file with relative path traversal
    let thriftContent = unlines
          [ "namespace hs \"" ++ traversal ++ "\""
          , "struct TestStruct { 1: i32 value }"
          ]
    writeFile inputFile thriftContent

    -- Create nested output directory
    createDirectoryIfMissing True outputPath

    -- Compile the thrift file - should throw NamespacePathValidationError
    let opts = (defaultOptions defaultHsOpts)
          { optsPath = inputFile
          , optsOutPath = outputPath
          , optsRecursive = True
          , optsGenMode = EmitCode
          }
    result <- try @NamespacePathValidationError $ do
      (headModule, _deps) <- typecheckInput opts =<< parseAll opts inputFile
      genHsCode opts headModule

    case result of
      Left NamespacePathValidationError{} ->
        return ()  -- ✓ Expected validation error

      Right _modules ->
        assertFailure
          "Compilation succeeded but should have failed validation"
