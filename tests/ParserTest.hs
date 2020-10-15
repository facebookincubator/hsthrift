--
-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements. See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership. The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- License); you may not use this file except in compliance
-- with the License. You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing,
-- software distributed under the License is distributed on an
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied. See the License for the
-- specific language governing permissions and limitations
-- under the License.
--

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
