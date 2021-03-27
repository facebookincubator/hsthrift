-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TypeApplications #-}
module TestFixtures (main) where

import Control.Monad (unless, when)
import Data.Aeson.Encode.Pretty
import Data.Char
import Data.List
import Data.List.Extra
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import Data.Typeable
import System.FilePath
import Test.HUnit
import TestRunner
import Text.Printf

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
  let parseResult = parseFileContentsWithExts exts tmContents
  when (canParse tmPath) $
    assertBool
      (printf "Test fixture for file '%s' parses" tmPath)
      (isOk parseResult)

  fixture <- readFile tmPath
  assertEqualPgm tmPath fixture tmContents
  where
    mname = intercalate "/" $ dropLower $ wordsBy (== '/') tmPath
    dropLower [] = []
    dropLower [x] = [x] -- Don't drop the last thing
    dropLower (x : xs) | lowerName x = dropLower xs
                       | otherwise   = x : dropLower xs
    lowerName [] = True -- We want to drop emptys
    lowerName (c : _) = isLower c
    canParse filename
      | isSuffixOf ".ast" filename = False
      | isSuffixOf "Service.hs" filename = False
      -- Service.hs does not parse due to
      -- https://github.com/haskell-suite/haskell-src-exts/issues/310
      -- (I think)
      | otherwise = True
    isOk (ParseOk _) = True
    isOk (ParseFailed _ _) = False
    exts = map EnableExtension [ConstraintKinds]

assertEqualPgm :: String -> String -> String -> IO ()
assertEqualPgm path expected obtained =
  unless (normalize expected == normalize obtained) $ do
    putStrLn path
    putStrLn "Expected:"
    putStrLn expected
    putStrLn "-------------------------------------------------------------------"
    putStrLn "But got:"
    putStrLn obtained
    error "expectation failure"
  where
    -- In dependent-sum > 0.6 the This constructor was renamed to Some
    normalize
      = Text.unpack
      . Text.unlines
      . map fixLine
      . Text.lines
      . Text.pack

    fixLine l
      = Text.replace "\"compiler/test/fixtures/"
                     "\"test/fixtures/"
      . Text.replace "\"include_path\": \"compiler\","
                     "\"include_path\": \".\","
    -- TODO: update the fixtures and remove this replace once 8.8 lands
      $ Text.replace "Thrift.This" "Thrift.Some" l
