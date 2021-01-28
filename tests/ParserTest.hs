-- Copyright (c) Facebook, Inc. and its affiliates.

module ParserTest where

import Test.HUnit
import TestRunner

import Thrift.Compiler.Parser
import Thrift.Compiler.Types

structTest :: Test
structTest = TestLabel "struct test"  $ TestCase $ do
  let input = unlines
              [ "struct Foo {"
              , "  1: optional i32 foo,"
              , "  2: Bar bar;"
              , "}"
              ]
  let decls = snd <$> runParser parseThrift "" input
  case decls of
    Right [ D_Struct Struct{..} ]
      | "Foo" <- structName
      , [ Field{fieldId=1,fieldName="foo",fieldType=AnnotatedType I32 _ _}
        , Field{fieldId=2,fieldName="bar",fieldType=AnnotatedType (TNamed "Bar") _ _}
        ] <- structMembers -> return ()
    _ -> assertFailure "not equal"

typedefTest :: Test
typedefTest = TestLabel "typedef test" $ TestCase $ do
  let input = "typedef map<string, list<Bar>> Foo"
  let decls = snd <$> runParser parseThrift "" input
  case decls of
    Right [D_Typedef Typedef{..}]
      | "Foo" <- tdName
      , AnnotatedType
        (TMap
         (AnnotatedType TText _ _)
         (AnnotatedType (TList (AnnotatedType (TNamed "Bar") _ _)) _ _))
        _ _ <- tdType -> return ()
    _ -> assertFailure "not equal"

priorityTest :: Test
priorityTest = TestLabel "priority test" $ TestCase $ do
  let input = "service S { void foo() (priority = \"HIGH\") }"
  let decls = snd <$> runParser parseThrift "" input
  case decls of
    Right [D_Service Service{serviceFunctions=[Function{..}]}] ->
      case getAnns funAnns of
       [ValueAnn{..}]
         | vaTag == "priority"
         , TextAnn p _ <- vaVal -> assertEqual "priority" "HIGH" p
       _ -> assertFailure "Parsing failed."
    _ -> assertFailure "Parsing failed."

main :: IO ()
main = testRunner $ TestList [ structTest, typedefTest, priorityTest]
