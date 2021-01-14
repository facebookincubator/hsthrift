-- Copyright (c) Facebook, Inc. and its affiliates.

module RoundTripTest (main) where

import Control.Monad
import qualified Data.Text.Lazy as Text
import Data.List.Extra
import System.Directory
import System.FilePath
import System.Posix.Files
import Test.HUnit
import TestRunner

import Thrift.Compiler.Parser
import Thrift.ExactPrint.Convert
import Thrift.ExactPrint.PrettyPrint

headerSize :: Int
headerSize = 4

-- Not all tests from fbthrift currently pass. Remove from this list to see the
-- error message
failingTests :: [FilePath]
failingTests =
  [ "common/hs/thrift/exactprint/tests/fbthrift-tests/basic-annotations/src/module.thrift"
  , "common/hs/thrift/exactprint/tests/fbthrift-tests/hack-const-collections/src/module.thrift"
  , "common/hs/thrift/exactprint/tests/fbthrift-tests/hack-const-collections/src/module.thrift"
  , "common/hs/thrift/exactprint/tests/fbthrift-tests/interactions/src/module.thrift"
  , "common/hs/thrift/exactprint/tests/fbthrift-tests/interactions/src/module.thrift"
  , "common/hs/thrift/exactprint/tests/fbthrift-tests/json_experimental/src/ThriftdocTest.thrift"
  , "common/hs/thrift/exactprint/tests/fbthrift-tests/map_construct/src/module.thrift"
  , "common/hs/thrift/exactprint/tests/fbthrift-tests/map_construct/src/module.thrift"
  , "common/hs/thrift/exactprint/tests/fbthrift-tests/php-migration/src/module.thrift"
  , "common/hs/thrift/exactprint/tests/fbthrift-tests/php-migration/src/module.thrift"
  , "common/hs/thrift/exactprint/tests/fbthrift-tests/py-future/src/test.thrift"
  , "common/hs/thrift/exactprint/tests/fbthrift-tests/py-future/src/test.thrift"
  , "common/hs/thrift/exactprint/tests/fbthrift-tests/sink/src/module.thrift"
  , "common/hs/thrift/exactprint/tests/fbthrift-tests/sink/src/module.thrift"
  , "common/hs/thrift/exactprint/tests/fbthrift-tests/stream/src/module.thrift"
  ]

fileLineStr :: FilePath -> Int -> String
fileLineStr path line = path ++ ":" ++ show line

traverseDir :: FilePath -> IO [FilePath]
traverseDir top = do
  ds <- getDirectoryContents top
  paths <- forM (filter (not . isPrefixOf ".") ds) $ \d -> do
    let path = top </> d
    s <- getFileStatus path
    if isDirectory s
      then traverseDir path
      else return [path]
  return $ concat paths

roundTripTest :: FilePath -> FilePath -> Test
roundTripTest fbcode path = TestLabel ("Round Trip: " ++ path) $ TestCase
  $ do
    input <- withCurrentDirectory fbcode $ readFile path
    -- skip header
    let inputLines = drop headerSize $ lines input
    case runParser parseThrift path (unlines inputLines) of
      Left e -> fail e
      Right parsedData -> do
        forM_ (zip3 [1+headerSize..] inputLines outputLines) $
          \(line, inputLine, outputLine) ->
            assertEqual
              ("roundtrip line mismatch: " ++ fileLineStr path line)
              inputLine
              outputLine

        assertEqual "roundtrip file length"
          (length inputLines)
          (length outputLines)

        where
          outputLines = lines output
          output =
            Text.unpack $
              exactPrintThrift $
              computeThriftFileOffsets $
              mkThriftFile parsedData

  where
    mkThriftFile (headers, decls) = ThriftFile
      { thriftName    = ""
      , thriftPath    = ""
      , thriftHeaders = headers
      , thriftDecls   = decls
      , thriftSplice  = ()
      , thriftComments = []
      }


roundTripTests :: IO Test
roundTripTests = do
  dir <- getCurrentDirectory
  let
    fbcode  = fst $ breakOnEnd "fbcode" dir
    testPath = "common/hs/thrift/exactprint/tests/fbthrift-tests"

  (testFiles::[String]) <- traverseDir testPath
  let
    testList =
      testFiles >>= \(path::String) ->
        if | path `elem` failingTests -> []
           | otherwise -> [roundTripTest fbcode path]

  return $ TestList testList



main :: IO ()
main = testRunner =<< roundTripTests
