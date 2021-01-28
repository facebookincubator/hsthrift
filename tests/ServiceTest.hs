-- Copyright (c) Facebook, Inc. and its affiliates.

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module ServiceTest where

import Data.Either.Combinators
import Test.HUnit
import TestRunner
import qualified Data.Map.Strict as Map

import Thrift.Compiler.Options
import Thrift.Compiler.Parser
import Thrift.Compiler.Plugins.Linter
import Thrift.Compiler.Typechecker
import Thrift.Compiler.Typechecker.Monad
import Thrift.Compiler.Types

-- This import just makes sure the file compiles
import Service.Types

-- Invalid Inputs --------------------------------------------------------------

superCycle :: Test
superCycle = expectTypeError "super service cycle" $ unlines
  [ "service X extends Y {}"
  , "service Y extends X {}"
  ]

unknownService :: Test
unknownService = expectTypeError "reference service that doesn't exist"
  "service X extends Y {}"

unknownType :: Test
unknownType = expectTypeError "unknown type" $ unlines
  [ "service X {"
  , "  void myFunction(1: Y notARealType)"
  , "}"
  ]

duplicateFunction :: Test
duplicateFunction = expectTypeError "duplicate function" $ unlines
  [ "service X {"
  , "  void myFunction();"
  , "  i64 myFunction();"
  , "}"
  ]

duplicateFunctionSuper :: Test
duplicateFunctionSuper = expectTypeError "duplicate function super" $ unlines
  [ "service X {"
  , "  void myFunction()"
  , "}"
  , "service Y extends X {"
  , "  i64 myFunction()"
  , "}"
  ]

optionalArgument :: Test
optionalArgument = expectParseError "optional argument" $ unlines
  [ "service X {"
  , "  void myFunction(1: optional i64 x)"
  , "}"
  ]

duplicateArgIds :: Test
duplicateArgIds = expectTypeError "duplicate argument ids" $ unlines
  [ "service X {"
  , "  void myFunction(1: i32 x, 1: i64 y)"
  , "}"
  ]

duplicateExcIds :: Test
duplicateExcIds = expectTypeError "duplicate exception ids" $ unlines
  [ "exception A {}"
  , "exception B {}"
  , "service X {"
  , "  void myFunction() throws(1: A a, 1: B b)"
  , "}"
  ]

--------------------------------------------------------------------------------

data Error = ParseError String | TypeErrors [TypeError Linter]

parseAndTypecheck
  :: String -> Either Error (Program Linter Loc, [Program Linter Loc])
parseAndTypecheck input =
  case runParser parseThrift "<untitled>" input of
    Left err -> Left $ ParseError err
    Right tFile ->
      mapLeft TypeErrors $ typecheck (defaultOptions NoOpts) (mkModuleMap tFile)

mkModuleMap :: ([Header Loc], [Parsed Decl]) -> ModuleMap
mkModuleMap (headers, decls) =
  Map.singleton "" ThriftFile
    { thriftName = ""
    , thriftPath = ""
    , thriftHeaders = headers
    , thriftDecls   = decls
    , thriftSplice = Nothing
    , thriftComments = []
    }

expectTypeError :: String -> String -> Test
expectTypeError label input = TestLabel label $ TestCase $
  case parseAndTypecheck input of
    Left TypeErrors{} -> return ()
    _ -> assertFailure "should be a type error"

expectParseError :: String -> String -> Test
expectParseError label input = TestLabel label $ TestCase $
  case parseAndTypecheck input of
    Left ParseError{} -> return ()
    _ -> assertFailure "should be a parse error"

main :: IO ()
main = testRunner $ TestList
  [ superCycle
  , unknownService
  , unknownType
  , duplicateFunction
  , duplicateFunctionSuper
  , optionalArgument
  , duplicateArgIds, duplicateExcIds
  ]
