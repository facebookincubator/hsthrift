module Util (withFixtureOptions) where

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
  let
    fbcode  = fst $ breakOnEnd "fbcode" dir
    outPath = "common/hs/thrift/compiler/test/fixtures"
  -- Run in the fbcode directory so that we can use relative paths
  withCurrentDirectory fbcode $ f
    [ TheseOptions (defaultOptions langopts)
         { optsPath = path
         , optsOutPath = outPath </> genDir
         , optsRecursive = True
         , optsGenMode = mode
         , optsSingleOutput = singleOut
         }
    | (TheseLangOpts langopts, mode, singleOut, genDir, path) <-
         [ (TheseLangOpts defaultHsOpts, EmitCode, False, "",
            "common/hs/thrift/tests/if/hs_test.thrift")
         , (TheseLangOpts defaultHsOpts{hsoptsExtraHasFields=True},
            EmitCode, False, "",
            "common/hs/thrift/tests/if/service.thrift")
         , (TheseLangOpts defaultHsOpts, EmitCode, False, "",
            "common/hs/thrift/compiler/test/if/a.thrift")
         , (TheseLangOpts NoOpts, EmitJSON WithoutLoc, False, "gen-basic",
            "common/hs/thrift/compiler/test/if/a.thrift")
         , (TheseLangOpts NoOpts, EmitJSON WithoutLoc, True, "gen-single-out",
            "common/hs/thrift/compiler/test/if/a.thrift")
         , (TheseLangOpts NoOpts, EmitJSON WithLoc, False, "gen-basic-loc",
            "common/hs/thrift/compiler/test/if/a.thrift")
         , (TheseLangOpts NoOpts, EmitJSON WithLoc, True, "gen-single-out-loc",
            "common/hs/thrift/compiler/test/if/a.thrift")
         ]
    ]
