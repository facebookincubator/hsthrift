-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE NamedFieldPuns #-}
module TypecheckerTestLenient where

import Data.Text ( Text )
import qualified Data.Text as Text

import Test.HUnit
import TestRunner
import qualified Data.Map.Strict as Map

import Thrift.Compiler.Options
import Thrift.Compiler.Parser
import Thrift.Compiler.Plugin
import Thrift.Compiler.Plugins.Linter
import Thrift.Compiler.Pretty ( renderTypeError )
import Thrift.Compiler.Types as T
import Thrift.Compiler.Typechecker
import Thrift.Compiler.Typechecker.Monad

mkModuleMap :: Text -> ([Header Loc], [Parsed Decl]) -> ModuleMap
mkModuleMap name (headers, decls) =
  let path = Text.unpack name ++ ".thrift"
  in Map.singleton path ThriftFile
      { thriftName = name
      , thriftPath = path
      , thriftHeaders = headers
      , thriftDecls   = decls
      , thriftSplice = Nothing
      , thriftComments = []
      }

lenientOptions :: LangOpts l -> Bool -> Options l
lenientOptions opts optsLenient = (defaultOptions opts){ optsLenient }

typecheckIncludes
  :: Typecheckable l
  => LangOpts l
  -> Bool
  -> [(Text, String)]
  -> Either String (Either [TypeError l] (Program l Loc, [Program l Loc]))
typecheckIncludes opts lenient files =
    case parseIncludes files of
      Left err -> Left ("parse error: " ++ err)
      Right moduleMap -> Right $
        typecheck (lenientOptions opts lenient) moduleMap
  where
    parseInclude :: (Text, String) -> Either String ModuleMap
    parseInclude (name, input) =
      let path = Text.unpack name ++ ".thrift"
      in case runParser parseThrift path input of
          Left err -> Left ("parse error: " ++ path ++ " : " ++ err)
          Right hsds -> Right (mkModuleMap name hsds)

    parseIncludes :: [(Text, String)] -> Either String ModuleMap
    parseIncludes = fmap mconcat . traverse parseInclude

typecheckIncludesTest :: String -> [(Text, String)] -> Test
typecheckIncludesTest label files =
  TestLabel (label ++ " should pass") $ TestCase $
    case typecheckIncludes NoOpts True files of
      Right (Right _) -> return ()
      Right (Left errors) -> assertFailure $
        "should have typechecked: " <> show (map renderTypeError errors)
      Left err -> assertFailure err

typecheckIncludesErrorTest :: String -> [(Text, String)] -> Test
typecheckIncludesErrorTest label files =
  TestLabel (label ++ " should fail") $ TestCase $
    case typecheckIncludes NoOpts False files of
      Right (Left errors)
        | any isTypeError errors -> return ()
        | otherwise -> assertFailure $
            "wrong errors: " <> show (map renderTypeError errors)
      Right (Right _) -> assertFailure "should be a type error but typechecked"
      Left err -> assertFailure err

parseAndTypecheck
  :: Typecheckable l
  => LangOpts l
  -> Bool
  -> String
  -> Either String (Either [TypeError l] (Program l Loc, [Program l Loc]))
parseAndTypecheck opts lenient input =
  case runParser parseThrift "ttl.thrift" input of
    Left err -> Left ("parse error: " ++ err)
    Right hsds -> Right $
      typecheck (lenientOptions opts lenient) (mkModuleMap "ttl" hsds)

typecheckTest :: String -> String -> Test
typecheckTest label input = TestLabel (label ++ " should pass") $ TestCase $
  case parseAndTypecheck NoOpts True input of
    Right (Right _) -> return ()
    Right (Left errors) -> assertFailure $
      "should have typechecked: " <> show (map renderTypeError errors)
    Left err -> assertFailure err

typeErrorTest :: String -> String -> Test
typeErrorTest label input = TestLabel (label ++ " should fail") $ TestCase $
  case parseAndTypecheck NoOpts False input of
    Right (Left errors)
      | any isTypeError errors -> return ()
      | otherwise -> assertFailure $
          "wrong errors: " <> show (map renderTypeError errors)
    Right (Right _) -> assertFailure "should be a type error but typechecked"
    Left err -> assertFailure err

isTypeError :: TypeError l -> Bool
isTypeError TypeError{} = True
isTypeError EmptyInput = False
isTypeError CyclicModules{} = False

--------------------------------------------------------------------------------

emptySet1 :: String
emptySet1 = unlines
  [ "struct Foo {"
  , "  2: set<i32> emptySet = {},"
  , "}"
  ]

emptySet2 :: String
emptySet2 = unlines [ "const set<i32> EmptySet = {}" ]

enumToIntDefaults :: String
enumToIntDefaults = unlines
  [ "enum Bar {"
  , "  A = 1,"
  , "  B = 2,"
  , "  C = 3,"
  , "}"
  , ""
  , "struct Foo {"
  , "  1: i32 enumToInt = A,"
  , "  2: list<i16> enumToIntList = [A, B, C],"
  , "  3: set<byte> enumToIntSet = [A, B, C],"
  , "  4: map<i64, bool> enumToIntMapKey = { A : true, B : false, C : true },"
  , "  5: map<string, i16> enumToMapValue = "
  , "    { \"7\" : A, \"8\" : B, \"9\": C },"
  , "}"
  ]

enumToIntConst :: String
enumToIntConst = unlines
  [ "enum Bar {"
  , "  A = 1,"
  , "  B = 2,"
  , "  C = 3,"
  , "}"
  , ""
  , "const i32 enumToInt = A"
  , "const list<i16> enumToIntList = [A, B, C]"
  , "const set<byte> enumToIntSet = [A, B, C]"
  , "const map<i64, bool> enumToIntMapKey = { A : true, B : false, C : true }"
  , "const map<string, i16> enumToMapValue = "
  , "    { \"7\" : A, \"8\" : B, \"9\": C }"
  ]

enumConstToIntConst :: String
enumConstToIntConst = unlines
  [ "enum Bar {"
  , "  A = 1,"
  , "  B = 2,"
  , "  C = 3,"
  , "}"
  , ""
  , "const Bar iAmB = B"
  , "typedef i16 Foo"
  , "const Foo fooIs2 = iAmB"
  ]

selfQualification :: String
selfQualification = unlines
  [ "enum Bar {"
  , "  A = 1,"
  , "  B = 2,"
  , "  C = 3,"
  , "}"
  , ""
  , "const ttl.Bar iAmB = ttl.Bar.B"
  ]

ignoreWrongEnumKeys :: String
ignoreWrongEnumKeys = unlines
  [ "enum Bar {"
  , "  A = 1,"
  , "  B = 2,"
  , "  C = 3,"
  , "}"
  , "enum Wrong {"
  , "  WrongA = 1"
  , "  WrongB = 10"
  , "  WrongC = 100"
  , "}"
  , "const map<Bar, i16> barMap = {"
  , "  A : 1,"
  , "  B : 2,"
  , "  C : 3,"
  , "  WrongA : 11,"
  , "  WrongB : 12,"
  , "  WrongC : 13,"
  , "}"
  ]

noAltUnion :: String
noAltUnion = unlines
  [ "union Zero {"
  , "}"
  ]

emptyListMap :: String
emptyListMap = unlines
  [ "const list<i32> EmptyList = {}"
  , "const map<i32,i32> EmptyMap = []"
  ]

selfQualType :: String
selfQualType = unlines
  [ "struct Foo {"
  , "  1: string name,"
  , "}"
  , "service S {"
  , "  ttl.Foo fun("
  , "    1: ttl.Foo param1,"
  , "  )"
  , "}"
  ]

lenientInputs :: [(String, String)]
lenientInputs =
  [ ( "T43181705 empty set1", emptySet1 )
  , ( "T43181705 empty set2", emptySet2 )
  , ( "T43181363 enum to Int Defaults", enumToIntDefaults )
  , ( "T43181363 enum to Int Constants", enumToIntConst )
  , ( "T43181363 enum Const to Int Constant", enumConstToIntConst )
  , ( "T43181635 self qualification", selfQualification )
  , ( "T45688659 ignore wrong enum keys", ignoreWrongEnumKeys )
  , ( "T46325195 allow unions without alts", noAltUnion )
  , ( "T43181705 empty list/map", emptyListMap )
  , ( "T43181635 self qualified type", selfQualType )
  ]

lenientTests :: Test
lenientTests = TestList $
  map (uncurry typecheckTest) lenientInputs
  ++
  map (uncurry typeErrorTest) lenientInputs
--------------------------------------------------------------------------------

includeA :: (Text, String)
includeA = ("file_a", "typedef i64 A")

includeB :: (Text, String)
includeB = ("file_b", "include \"file_a.thrift\"")

transitiveC :: (Text, String)
transitiveC = (,) "file_c" $ unlines
  [ "include \"file_b.thrift\""
  , "const file_a.A deep = 17"
  ]

transitiveInputs :: (String, [(Text, String)])
transitiveInputs =
  ("T43181464 transitive import", [ transitiveC, includeB, includeA ])

lenientTransitiveInclude :: Test
lenientTransitiveInclude = TestList
  [ uncurry typecheckIncludesTest transitiveInputs
  , uncurry typecheckIncludesErrorTest transitiveInputs
  ]

--------------------------------------------------------------------------------

main :: IO ()
main = testRunner $ TestList [ lenientTests, lenientTransitiveInclude ]
