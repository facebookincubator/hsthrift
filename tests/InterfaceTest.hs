-- Copyright (c) Facebook, Inc. and its affiliates.

module InterfaceTest where

import Data.Bifunctor
import Data.List
import Test.HUnit
import TestRunner
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (Text)

import Language.Haskell.Exts.Syntax
import Language.Haskell.Names

import Thrift.Compiler.Options
import Thrift.Compiler.Parser
import Thrift.Compiler.Plugin
import Thrift.Compiler.Plugins.Haskell

srcFile :: String
srcFile = unlines
  [ "struct A {"
  , "  1: i64 foo"
  , "  2: string bar"
  , "}"
  , ""
  , "typedef i64 B"
  , "typedef i64 C (hs.newtype)"
  , ""
  , "enum D { x = 0; y = 1; z = 1 }"
  , ""
  , "union E {"
  , "  1: i64 a"
  , "  2: i64 b"
  , "}"
  , ""
  , "const i64 XXX = 0"
  ]

interfaceTest :: Test
interfaceTest = TestLabel "Interface Test" $ TestCase $ do
  let
    file = "foo_bar"
    tf = case runParser parseThrift (file ++ ".thrift") srcFile of
      Left e -> error e
      Right (headers, decls) -> ThriftFile
        { thriftName = Text.pack file
        , thriftPath = file ++ ".thrift"
        , thriftHeaders = headers
        , thriftDecls   = decls
        , thriftSplice = Nothing
        , thriftComments = []
        }
    HsInterface env' rmap' = getInterface (defaultOptions defaultHsOpts) tf

    env = sort $ map fst assocs
    rmap = Map.fromList assocs
    assocs :: [(String,Text)]
    assocs = map (bimap ("FooBar.Types." ++) ("foo_bar." <>))
      [ ("A", "A")
      , ("A", "A")
      , ("a_foo", "A")
      , ("a_bar", "A")
      , ("B", "B")
      , ("C", "C")
      , ("C", "C")
      , ("unC", "C")
      , ("D", "D")
      , ("D_x", "D")
      , ("D_y", "D")
      , ("D_z", "D")
      , ("E", "E")
      , ("E_a","E")
      , ("E_b", "E")
      , ("xXX", "XXX")
      ]
  assertEqual "env" (Just env) $
    sort . map ppSymbol <$> Map.lookup (ModuleName () "FooBar.Types") env'
  assertEqual "rmap" rmap $ Map.mapKeys ppSymbol rmap'

main :: IO ()
main = testRunner interfaceTest
