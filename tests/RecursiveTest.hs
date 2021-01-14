-- Copyright (c) Facebook, Inc. and its affiliates.

module RecursiveTest where

import Test.HUnit
import TestRunner
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Thrift.Compiler.Options
import Thrift.Compiler.Parser
import Thrift.Compiler.Plugins.Haskell
import Thrift.Compiler.Typechecker
import Thrift.Compiler.Types

moduleA :: (String, String)
moduleA = ("A",) $ unlines
  [ "include \"B.thrift\""
  , ""
  , "struct A {}"
  , "struct A2 {}"
  ]

moduleB :: (String, String)
moduleB = ("B",) $ unlines
  [ "struct B {}"
  , "struct B2 {}"
  , "struct B3 {}"
  ]

parseMod :: String -> String -> (FilePath, ThriftFile SpliceFile Loc)
parseMod name contents = (path,) ThriftFile
  { thriftName = Text.pack name
  , thriftPath = path
  , thriftHeaders = headers
  , thriftDecls   = decls
  , thriftSplice = Nothing
  , thriftComments = []
  }
  where
    path = name ++ ".thrift"
    (headers, decls) =
      either error id $ runParser parseThrift path contents

recursiveReqSymTest :: Test
recursiveReqSymTest = TestLabel "recursive required symbols" $ TestCase $ do
  let
    input = Map.fromList $ map (uncurry parseMod) [moduleA, moduleB]
    opts = (defaultOptions defaultHsOpts)
      { optsPath       = "A.thrift"
      , optsRecursive  = True
      , optsReqSymbols = Just ["A", "B.B"]
      }
  case typecheck opts input of
    Right (p, ps) -> do
      let
        structs =
          [ structName
          | Program{..} <- p : ps
          , D_Struct Struct{..} <- progDecls
          ]
      assertEqual "req symbols worked" ["A", "B"] structs
    Left{} -> assertFailure "type error"

main :: IO ()
main = testRunner $ TestList [ recursiveReqSymTest ]
