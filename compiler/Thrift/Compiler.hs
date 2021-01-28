-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TypeApplications #-}
module Thrift.Compiler
  ( run
  , parseAll, parseThriftFile
  , parseAllE, parseThriftFileE
  , typecheckInput
  , typecheckInputE
  ) where

import Control.Concurrent.Async
import Control.Monad
import Data.List
import Data.Typeable
import Language.Haskell.Exts hiding (parse, Decl, name)
import System.Exit
import System.FilePath
import qualified Data.Map.Strict as Map

-- Backends
import Thrift.Compiler.GenHaskell
import Thrift.Compiler.GenJSON
import Thrift.Compiler.GenJSONLoc

import Thrift.Compiler.Options
import Thrift.Compiler.OptParse
import Thrift.Compiler.Parser
import Thrift.Compiler.Pretty
import Thrift.Compiler.Typechecker
import Thrift.Compiler.Typechecker.Monad
import Thrift.Compiler.Types as Thrift

-- | Run the thrift compiler and return a list of paths to the generated files
run :: ThriftLanguage l => Options l -> IO [FilePath]
run opts@Options{..} = do
  -- Parse and Typecheck
  (headModule, deps) <- typecheckInput opts =<< parseAll opts optsPath

  if optsLenient then return []
  else do
    -- Generate Outputs
    genFiles <- getGenerator opts headModule deps

    -- Write THIRFT-MADE-GENERATED-FILES
    case optsThriftMade of
      Nothing -> return ()
      Just path -> writeFile path $ unlines genFiles

    return genFiles

typecheckInput
  :: ThriftLanguage l
  => Options l
  -> ModuleMap
  -> IO (Program l Thrift.Loc, [Program l Thrift.Loc])
typecheckInput opts moduleMap =
  case typecheckInputE opts moduleMap of
    Left es -> mapM_ putStrLn es >> exitFailure
    Right ms -> pure ms

typecheckInputE
  :: ThriftLanguage l
  => Options l
  -> ModuleMap
  -> Either [String] (Program l Thrift.Loc, [Program l Thrift.Loc])
typecheckInputE opts moduleMap =
  case typecheck opts moduleMap of
    Left es ->  Left $ map renderTypeErrorPlain (sortBy orderError es)
    Right ms -> Right ms

getGenerator
  :: ThriftLanguage l
  => Options l
  -> Program l Thrift.Loc
  -> [Program l Thrift.Loc]
  -> IO [FilePath]
getGenerator opts@Options{..} prog deps = case optsGenMode of
  Lint -> return []
  EmitCode
    | Just (hsopts, hsprogs) <- cast (opts, allProgs) ->
        concat <$> mapConcurrently @[] (writeHsCode hsopts) hsprogs
    | otherwise -> error
        "Code generation is only supported for Haskell. Try using --emit-json"
  EmitJSON WithoutLoc
    | optsSingleOutput -> (:[]) <$> writeJSON prog mdeps
    | otherwise -> mapConcurrently (`writeJSON` Nothing) allProgs
  EmitJSON WithLoc
    | optsSingleOutput -> (:[]) <$> writeJSONLoc prog mdeps
    | otherwise -> mapConcurrently (`writeJSONLoc` Nothing) allProgs
  where
    (allProgs, mdeps)
      | optsRecursive = (prog : deps, Just deps)
      | otherwise = ([prog], Nothing)

-- | Return a parse error or the result
parseAllE :: Options l -> FilePath -> IO (Either String ModuleMap)
parseAllE Options{..} = parseItE (Right Map.empty)
  where
    parseItE :: Either String ModuleMap
             -> FilePath -> IO (Either String ModuleMap)
    parseItE err@Left{} _path = pure err -- stop at first error
    parseItE (Right tmap) path
      -- If we already parsed it, don't do it again
      | Map.member path tmap = pure (Right tmap)
      | otherwise = do
          e <- parseThriftFileE optsIncludePath path
          case e of
            Left err -> return (Left err)
            Right file@ThriftFile{..} -> do
              let newMap = Map.insert path file tmap
              foldM parseItE (Right newMap) (getIncludes thriftHeaders)
    getIncludes = foldr getInc []
    getInc HInclude{incType=Include,..} ps = incPath : ps
    getInc _ ps = ps

-- | parse all the things recursively, starting with the input and traversing
-- the includes
parseAll :: Options l -> FilePath -> IO ModuleMap
parseAll options fp = do
  e <- parseAllE options fp
  case e of
    Left err -> putStrLn err >> exitFailure
    Right x -> return x

-- | Return a parse error or the result
parseThriftFileE
  :: FilePath -> FilePath
  -> IO (Either String (ThriftFile SpliceFile Thrift.Loc))
parseThriftFileE baseDir path = do
  result <- parse baseDir path
  case result of
    Left err -> return (Left err)
    Right file@ThriftFile{..} -> do
      spliceFile <- parseHsInclude baseDir thriftHeaders
      return (Right file { thriftSplice = spliceFile })

-- | Parse a single thrift file
parseThriftFile :: FilePath -> FilePath -> IO (ThriftFile SpliceFile Thrift.Loc)
parseThriftFile baseDir path = do
  e <- parseThriftFileE baseDir path
  case e of
      Left err -> putStrLn err >> exitFailure
      Right x -> return x

-- | Select the last hs_include header (there should be at most one)
parseHsInclude :: FilePath -> [Header a] -> IO SpliceFile
parseHsInclude baseDir headers = case foldl' getInc Nothing headers of
  Nothing -> return Nothing
  Just path -> Just <$> do
    let mode = defaultParseMode { parseFilename = path }
    fromParseResult <$> parseFileWithMode mode (baseDir </> path)
  where
    getInc _ HInclude{incType=HsInclude,..} = Just incPath
    getInc ns _ = ns
