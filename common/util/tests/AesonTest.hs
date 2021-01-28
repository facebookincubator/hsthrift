-- Copyright (c) Facebook, Inc. and its affiliates.

module AesonTest (main) where

import Test.HUnit
import TestRunner

import Data.Aeson hiding (decode)
import Data.Char
import qualified Data.Text as Text

import Util.Aeson

utf16SurrogatesTest :: Test
utf16SurrogatesTest = TestLabel "UTF-16 surrogates" . TestCase $ do
  assertEqual "U+FFFF" (Just $ pack [0xFFFF]) (decode "\"\\uffff\"")
  assertEqual "U+24B62" (Just $ pack [0x24B62]) (decode "\"\\uD852\\uDF62\"")
  assertEqual "Invalid" (Nothing :: Maybe Value) (decode "\"\\uDF62\\uD852\"")
  where
  pack = String . Text.pack . map chr
  decode = either (const Nothing) Just . parseValueStrict

main :: IO ()
main = testRunner $ TestList
  [ utf16SurrogatesTest
  ]
