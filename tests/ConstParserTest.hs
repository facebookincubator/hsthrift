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

module ConstParserTest where

import Test.HUnit
import TestRunner

import Thrift.Compiler.Parser
import Thrift.Compiler.Types

baseTypesTest :: Test
baseTypesTest = TestLabel "base types test" $ TestCase $ do
  let input = unlines
              [ "struct Foo {"
              , "  1: byte foo1 = 1"
              , "  2: i16  foo2 = 2"
              , "  3: i32  foo3 = 3"
              , "  4: i64  foo4 = 4"
              , "  5: float  foo3 = 3.14159"
              , "  6: double foo4 = 3.141592654"
              , "  7: string foo5 = 'hello world'"
              , "  8: bool   foo6 = false"
              , "}"
              ]
  let decls = snd <$> runParser parseThrift "" input
  case decls of
    Right
      [ D_Struct Struct
        { structName = "Foo"
        , structMembers =
          [ Field {fieldId=1,fieldVal=Just (UntypedConst _ (IntVal 1 _))}
          , Field {fieldId=2,fieldVal=Just (UntypedConst _ (IntVal 2 _))}
          , Field {fieldId=3,fieldVal=Just (UntypedConst _ (IntVal 3 _))}
          , Field {fieldId=4,fieldVal=Just (UntypedConst _ (IntVal 4 _))}
          , Field {fieldId=5,fieldVal=Just (UntypedConst _ (DoubleVal 3.14159 _))}
          , Field {fieldId=6,fieldVal=Just (UntypedConst _ (DoubleVal 3.141592654 _))}
          , Field {fieldId=7,fieldVal=Just (UntypedConst _ (StringVal "hello world" _))}
          , Field {fieldId=8,fieldVal=Just (UntypedConst _ (BoolVal False))}
          ]
        }
      ] -> return ()
    _ -> assertFailure "not equal"

listTest :: Test
listTest = TestLabel "simple list test" $ TestCase $ do
  let input = unlines
              [ "struct Foo {"
              , "  1: list<i32> foo1 = [1,2,3]"
              , "}"
              ]
  let decls = snd <$> runParser parseThrift "" input
  case decls of
    Right
      [ D_Struct Struct
        { structName = "Foo"
        , structMembers =
          [ Field
            { fieldId = 1
            , fieldVal = Just
               (UntypedConst _
                (ListVal
                 { lvElems =
                   [ ListElem { leElem=UntypedConst _ (IntVal 1 _) }
                   , ListElem { leElem=UntypedConst _ (IntVal 2 _) }
                   , ListElem { leElem=UntypedConst _ (IntVal 3 _) }
                   ]
                 }))
            }
          ]
        }
      ] -> return ()
    _ -> assertFailure "not equal"

nestedListTest :: Test
nestedListTest = TestLabel "nested list test" $ TestCase $ do
  let input = unlines
              [ "struct Foo {"
              , "  1: list<list<i32>> foo1 = [[1,2], [3,4]]"
              , "}"
              ]
  let decls = snd <$> runParser parseThrift "" input
  case decls of
    Right
      [ D_Struct Struct
        { structName = "Foo"
        , structMembers =
          [ Field
            { fieldId = 1
            , fieldVal = Just
              (UntypedConst _
               (ListVal
                { lvElems =
                  [ ListElem
                    { leElem = UntypedConst _ ListVal
                      { lvElems =
                        [ ListElem { leElem = UntypedConst _ (IntVal 1 _) }
                        , ListElem { leElem = UntypedConst _ (IntVal 2 _) }
                        ]
                      }
                    }
                  , ListElem
                    { leElem = UntypedConst _ ListVal
                      { lvElems =
                        [ ListElem { leElem = UntypedConst _ (IntVal 3 _) }
                        , ListElem { leElem = UntypedConst _ (IntVal 4 _) }
                        ]
                      }
                    }
                  ]
                }))
            }
          ]
        }
      ] -> return ()
    _ -> assertFailure "not equal"

mapTest :: Test
mapTest = TestLabel "map test" $ TestCase $ do
  let input = unlines
              [ "struct Foo {"
              , "  1: map<string, list<bool>> foo1 = { 'x' : [], 'y' : [1,0] }"
              , "}"
              ]
  let decls = snd <$> runParser parseThrift "" input
  case decls of
    Right
      [ D_Struct Struct
        { structName = "Foo"
        , structMembers =
          [ Field
            { fieldId = 1
            , fieldVal =
              (Just
               (UntypedConst _ MapVal
                { mvElems =
                  [ ListElem
                    { leElem = MapPair
                      { mpKey = UntypedConst _ (StringVal "x" _)
                      , mpVal = UntypedConst _ (ListVal [] _)
                      }
                    }
                  , ListElem
                    { leElem = MapPair
                      { mpKey = UntypedConst _ (StringVal "y" _)
                      , mpVal = UntypedConst _ ListVal
                        { lvElems =
                          [ ListElem{leElem=UntypedConst _ (IntVal 1 _)}
                          , ListElem{leElem=UntypedConst _ (IntVal 0 _)}
                          ]
                        }
                      }
                    }
                  ]
                }))
            }
          ]
        }
      ] -> return ()
    _ -> assertFailure "not equal"


main :: IO ()
main = testRunner $ TestList
  [ baseTypesTest, listTest, nestedListTest, mapTest ]
