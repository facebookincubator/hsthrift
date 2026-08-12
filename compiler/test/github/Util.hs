{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util (withFixtureOptions) where

import Data.List
import System.Directory
import System.FilePath

import Thrift.Compiler.Options
import Thrift.Compiler.OptParse
import Thrift.Compiler.Plugins.Haskell
import Thrift.Compiler.Plugins.Linter

withFixtureOptions :: ([SomeOptions] -> IO a) -> IO a
withFixtureOptions f = do
  dir <- getCurrentDirectory
  compilerDir <- findCompilerDir dir
  let outPath = if inTree
        then "compiler" </> "test" </> "fixtures"
        else                "test" </> "fixtures"
      inTree = takeBaseName compilerDir == "compiler"
      testsCwd = if inTree then takeDirectory compilerDir else compilerDir
      includePathFor fp
        | inTree, "compiler/" `isPrefixOf` fp = "compiler"
        | "tests/" `isPrefixOf` fp = "tests"
        | otherwise = "."
      fixup fp
        | Just rest <- stripPrefix "compiler/" fp = rest
        | Just rest <- stripPrefix "tests/" fp = rest
        | otherwise = fp

  withCurrentDirectory testsCwd $ f $
    [ TheseOptions (defaultOptions langopts)
         { optsPath = fixup path
         , optsOutPath = outPath </> genDir
         , optsIncludePath = includePathFor path
         , optsRecursive = True
         , optsGenMode = mode
         , optsSingleOutput = singleOut
         }
    | (TheseLangOpts langopts, mode, singleOut, genDir, path) <-
         [ (TheseLangOpts defaultHsOpts, EmitCode, False, "",
            "tests/if/hs_test.thrift")
         , (TheseLangOpts defaultHsOpts{hsoptsExtraHasFields=True},
            EmitCode, False, "gen-hasfields",
            "tests/if/service.thrift")
         ] ++
         [ (TheseLangOpts defaultHsOpts, EmitCode, False, "", path)
         | path <- [
            "compiler/test/if/a.thrift",
            "compiler/test/if/b.thrift",
            "compiler/test/if/bidi.thrift",
            "compiler/test/if/c.thrift",
            "compiler/test/if/d.thrift",
            "compiler/test/if/e.thrift",
            "compiler/test/if/f.thrift",
            "compiler/test/if/g.thrift",
            "compiler/test/if/recursive_annotation.thrift"
           ]
         ] ++
         [ (TheseLangOpts NoOpts, EmitJSON WithoutLoc, False, "gen-basic",
            "compiler/test/if/a.thrift")
         , (TheseLangOpts NoOpts, EmitJSON WithoutLoc, True, "gen-single-out",
            "compiler/test/if/a.thrift")
         , (TheseLangOpts NoOpts, EmitJSON WithLoc, False, "gen-basic-loc",
            "compiler/test/if/a.thrift")
         , (TheseLangOpts NoOpts, EmitJSON WithLoc, True, "gen-single-out-loc",
            "compiler/test/if/a.thrift")
         , (TheseLangOpts defaultHsOpts, EmitCode, False, "gen-structured",
            "compiler/test/if/h.thrift")
         ]
    ]

findCompilerDir :: FilePath -> IO FilePath
findCompilerDir cwd = lookFor "thrift-compiler.cabal" cwd maxDepth >>= \mdir ->
  case mdir of
    Nothing -> error "findCompilerDir: reached max depth, couldn't find thrift-compiler.cabal"
    Just d  -> return d

  where lookFor _ _ (-1) = return Nothing
        lookFor file curDir depth = do
          existsHere <- doesFileExist (curDir </> file)
          if existsHere
            then return (Just curDir)
            else do xs <- listDirectory curDir
                    visitDirsIn xs file curDir depth
        visitDirsIn [] _ _ _ = return Nothing
        visitDirsIn (d:ds) file curDir depth = do
          dirExists <- doesDirectoryExist (curDir </> d)
          if dirExists
            then do mfp <- lookFor file (curDir </> d) (depth-1)
                    case mfp of
                      Nothing -> visitDirsIn ds file curDir depth
                      Just fp -> return (Just fp)
            else visitDirsIn ds file curDir depth
        maxDepth = 2 :: Int
