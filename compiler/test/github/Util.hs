module Util (withFixtureOptions) where

import Data.Foldable
import Data.List.Extra
import System.Directory
import System.FilePath

import Thrift.Compiler.Options
import Thrift.Compiler.OptParse
import Thrift.Compiler.Plugins.Haskell
import Thrift.Compiler.Plugins.Linter

withFixtureOptions :: ([SomeOptions] -> IO a) -> IO a
withFixtureOptions f = do
  dir <- getCurrentDirectory
  let fbcode  = fst $ breakOnEnd "fbcode" dir
  compilerDir <-
    if null fbcode
    then -- running from github repo
         findCompilerDir
    else -- running from FB source tree
         return (fbcode </> "common" </> "hs" </> "thrift" </> "compiler")
  let outPath          = "compiler/test/fixtures"
      thriftTestsDir   = compilerDir </> ".." </> "tests"
  withCurrentDirectory (compilerDir </> "..") $ f
    [ TheseOptions (defaultOptions langopts)
         { optsPath = path
         , optsOutPath = outPath </> genDir
         , optsRecursive = True
         , optsGenMode = mode
         , optsSingleOutput = singleOut
         }
    | (TheseLangOpts langopts, mode, singleOut, genDir, path) <-
         [ (TheseLangOpts defaultHsOpts, EmitCode, False, "",
            "tests/if/hs_test.thrift")
         , (TheseLangOpts defaultHsOpts{hsoptsExtraHasFields=True},
            EmitCode, False, "",
            "tests/if/service.thrift")
         , (TheseLangOpts defaultHsOpts, EmitCode, False, "",
            "compiler/test/if/a.thrift")
         , (TheseLangOpts NoOpts, EmitJSON WithoutLoc, False, "gen-basic",
            "compiler/test/if/a.thrift")
         , (TheseLangOpts NoOpts, EmitJSON WithoutLoc, True, "gen-single-out",
            "compiler/test/if/a.thrift")
         , (TheseLangOpts NoOpts, EmitJSON WithLoc, False, "gen-basic-loc",
            "compiler/test/if/a.thrift")
         , (TheseLangOpts NoOpts, EmitJSON WithLoc, True, "gen-single-out-loc",
            "compiler/test/if/a.thrift")
         ]
    ]

findCompilerDir :: IO FilePath
findCompilerDir = do
  asum [ readFile (dir </> "thrift-compiler.cabal") >> return dir
       | dir <- ["compiler", ".", "..", "../.."]
       ]
