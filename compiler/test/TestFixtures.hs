{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module TestFixtures (main) where

import Control.Monad (unless, when)
import Data.Aeson.Encode.Pretty
import Data.List
import Data.List.Extra
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import Data.Typeable
import System.Exit
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Test.HUnit
import TestRunner

import Util

import Thrift.Compiler
import Thrift.Compiler.GenHaskell
import Thrift.Compiler.GenJSON
import Thrift.Compiler.GenJSONLoc
import Thrift.Compiler.Options
import Thrift.Compiler.OptParse

import Language.Haskell.Exts

main :: IO ()
main = withFixtureOptions $ \opts -> do
  fixtures <- mapM genFixtures opts
  testRunner $ TestList $ map checkFixture $ concat fixtures

genFixtures :: SomeOptions -> IO [ThriftModule]
genFixtures (TheseOptions opts@Options{..}) = do
  (headModule, deps) <- typecheckInput opts =<< parseAll opts optsPath
  generator headModule deps
  where
    generator prog deps = case optsGenMode of
      EmitCode
        | Just (hsopts, ps) <- cast (opts, prog : deps) ->
            concat <$> mapM @[] (genHsCode hsopts) ps
      EmitJSON WithoutLoc
        | optsSingleOutput -> return [genJSONThriftModule (Just deps) prog]
        | otherwise -> return $ map (genJSONThriftModule Nothing) $ prog : deps
      EmitJSON WithLoc
        | optsSingleOutput -> return [genJSONLocThriftModule (Just deps) prog]
        | otherwise -> return $ map (genJSONLocThriftModule Nothing)
                              $ prog : deps
      _ -> return []

    prettyJSON = encodePretty' defConfig { confCompare = compare }

    genJSONThriftModule ds prog = ThriftModule
      { tmPath = uncurry (</>) $ getAstPath prog
      , tmContents =
          Text.unpack $ Text.decodeUtf8 $ prettyJSON $ genJSON prog ds
      , tmModuleName = ""
      }
    genJSONLocThriftModule ds prog = ThriftModule
      { tmPath = uncurry (</>) $ getAstPath prog
      , tmContents =
          Text.unpack $ Text.decodeUtf8 $ prettyJSON $ genJSONLoc prog ds
      , tmModuleName = ""
      }

checkFixture :: ThriftModule -> Test
checkFixture ThriftModule{..} = TestLabel mname $ TestCase $ do
  when (canParse tmPath) $ assertParseOk tmPath tmContents exts
  fixture <- readFile tmPath
  assertFileEqual tmPath fixture tmContents
  where
    mname = snd $ breakOnEnd "/test/fixtures/" tmPath
    canParse filename
      | isSuffixOf ".ast" filename = False
      | isSuffixOf "Service.hs" filename = False
      -- Service.hs does not parse due to
      -- https://github.com/haskell-suite/haskell-src-exts/issues/310
      -- (I think)
      | otherwise = True
    exts = map EnableExtension [ConstraintKinds]

assertParseOk :: FilePath -> String -> [Extension] -> IO ()
assertParseOk path contents exts =
  case parseFileContentsWithExts exts contents of
    ParseOk _ -> return ()
    ParseFailed loc str -> do
      putStrLn path
      putStrLn "Contents:"
      putStrLn contents
      assertFailure $ "Parse failed at [" ++ path ++ "] (" ++
        show (srcLine loc) ++ ":" ++ show (srcColumn loc) ++ "): " ++ str

assertFileEqual :: FilePath -> String -> String -> IO ()
assertFileEqual _path expected obtained =
  unless (normalize expected == normalize obtained) $ withSystemTempDirectory "diff" $ \dir -> do
    let expectPath = dir </> "expect"
        obtainPath = dir </> "obtain"
    writeFile expectPath (normalize expected)
    writeFile obtainPath (normalize obtained)
    let cp = proc "diff" ["-U", "10", expectPath, obtainPath]
    (ec, out, _err) <- readCreateProcessWithExitCode cp ""
    unless (ExitSuccess == ec) $ do
      assertFailure out
  where
    -- In dependent-sum > 0.6 the This constructor was renamed to Some
    normalize
      = Text.unpack
      . Text.unlines
      . map fixLine
      . Text.lines
      . Text.pack

    fixLine
      = Text.replace "\"compiler/test/fixtures/"
                     "\"test/fixtures/"
      . Text.replace "\"include_path\": \"compiler\","
                     "\"include_path\": \".\","
    -- TODO: update the fixtures and remove this replace once 8.8 lands
      . Text.replace "Thrift.This" "Thrift.Some"
