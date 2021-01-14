-- Copyright (c) Facebook, Inc. and its affiliates.

module BinaryParserTest where

import Control.Monad (replicateM)
import Control.Applicative

import Data.ByteString (ByteString)

import Prelude hiding (takeWhile)

import Test.HUnit
import TestRunner

import Thrift.Binary.Parser

expectParseSuccess :: (Eq a, Show a) => ByteString -> a -> Parser a -> IO ()
expectParseSuccess inp expectedOut parser =
  case parse parser inp of
    Right value | value == expectedOut -> return ()
    Right wrongOut -> assertFailure $
      "expected " ++ show expectedOut ++ ", got " ++ show wrongOut
    Left err -> assertFailure $ "parse error: " ++ err

mkTestParseSuccess
  :: (Eq a, Show a) => String -> ByteString -> a -> Parser a -> Test
mkTestParseSuccess name inp expectedOut parser =
  TestLabel name $ TestCase $ expectParseSuccess inp expectedOut parser

expectParseFailure :: ByteString -> Parser a -> IO ()
expectParseFailure inp parser =
  case parse parser inp of
    Left _ -> return ()
    Right _ -> assertFailure "parsing should fail"

mkTestParseFailure :: String -> ByteString -> Parser a -> Test
mkTestParseFailure name inp parser =
  TestLabel name $ TestCase $ expectParseFailure inp parser

testAnyWord8 :: Test
testAnyWord8 = mkTestParseSuccess "anyWord8" "\7" 7 anyWord8

testCombineParsers :: Test
testCombineParsers =
  mkTestParseSuccess "combine parsers" "\3\1\2\3" [1,2,3] parseList
  where
    parseList = do
      numElems <- anyWord8
      replicateM (fromIntegral numElems) anyWord8

testAlternative :: Test
testAlternative = TestLabel "backtracking" $ TestCase $ do
  expectParseSuccess "\1" 1 parseWord8OrInt32
  expectParseSuccess "\0\0\0\1" 1 parseWord8OrInt32
  where
    parseWord8OrInt32 :: Parser Int
    parseWord8OrInt32 =
      (word8 1 >> return 1) <|> (fromIntegral <$> getInt32be)

testSkipSpaces :: Test
testSkipSpaces =
  mkTestParseSuccess "skipSpaces" "  \t\7" 7 (skipSpaces >> anyWord8)

testTakeWhile :: Test
testTakeWhile =
  mkTestParseSuccess "takeWhile" "abc\7" "abc" (takeWhile (/= 7))

main :: IO ()
main = testRunner $ TestList
  [ mkTestParseSuccess "getByteString" "abcd" "abc" (getByteString 3)
  , testAnyWord8
  , mkTestParseFailure "anyWord8 not enough bytes" "" anyWord8
  , testCombineParsers
  , testAlternative
  , testSkipSpaces
  , testTakeWhile
  , mkTestParseSuccess "double" "10.2" (10.2 :: Double) double
  , mkTestParseSuccess "double scientific" "1.0e-9" (1.0e-9 :: Double) double
  , mkTestParseSuccess "double negative" "-1.0e-9" (-1.0e-9 :: Double) double
  ]
