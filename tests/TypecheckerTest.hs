-- Copyright (c) Facebook, Inc. and its affiliates.

module TypecheckerTest where

import Data.Int
import Data.Text (Text)
import Test.HUnit
import TestRunner
import qualified Data.Map.Strict as Map
import TextShow

import Thrift.Compiler.Options
import Thrift.Compiler.Parser
import Thrift.Compiler.Plugin
import Thrift.Compiler.Plugins.Haskell
import Thrift.Compiler.Plugins.Linter
import Thrift.Compiler.Types as T
import Thrift.Compiler.Typechecker
import Thrift.Compiler.Typechecker.Monad

mkThrift :: [Parsed Decl] -> ModuleMap
mkThrift decls =  Map.singleton "" ThriftFile
                  { thriftName = ""
                  , thriftPath = ""
                  , thriftHeaders = []
                  , thriftDecls = decls
                  , thriftSplice = Nothing
                  , thriftComments = []
                  }

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

parseAndTypecheck
  :: Typecheckable l
  => LangOpts l
  -> String
  -> Either [TypeError l] (Program l Loc, [Program l Loc])
parseAndTypecheck opts input =
  case runParser parseThrift "" input of
    Left err -> error err
    Right tFile -> typecheck (defaultOptions opts) (mkModuleMap tFile)

-- Name Resolution Tests -------------------------------------------------------

resolutionTests :: Test
resolutionTests =
  TestList [ simpleTest, cycleTest, annotationTest, unknownTest ]

simpleTest :: Test
simpleTest = TestLabel "simple test" $ TestCase $ do
  let decls = [ mkTypedef "X" $ AnnotatedType I8 Nothing (Arity0Loc nlc)
              , mkTypedef "Y" $ mkNamedType "X"
              , mkStruct "Foo"
                [ mkField 1 "foo" $
                  AnnotatedType
                  (TMap (mkNamedType "Y") (mkNamedType "Bar"))
                  Nothing
                  (Arity2Loc nlc nlc nlc nlc)
                ]
              , D_Enum Enum
                { enumName = "Bar"
                , enumResolvedName = ()
                , enumConstants = []
                , enumIsPseudo = ()
                , enumLoc = StructLoc nlc nlc nlc nlc
                , enumAnns = Nothing
                , enumSAnns = []
                , enumNoUnknown = ()
                }
              ]
  let result = typecheck (defaultOptions NoOpts) (mkThrift decls)
  case fst <$> result of
    Right ( Program
            { progDecls =
              [ D_Typedef Typedef{ tdResolvedType = I8, tdResolvedName = "X" }
              , D_Typedef Typedef
                { tdResolvedType =
                    TTypedef Name {resolvedName=UName "X"} I8 _loc0
                , tdResolvedName = "Y"
                }
              , D_Struct Struct
                { structName = "Foo"
                , structMembers =
                  [ Field
                    { fieldId = 1
                    , fieldName = "foo"
                    , fieldResolvedType =
                      TMap
                      (TTypedef Name {resolvedName=UName "Y"}
                       (TTypedef Name {resolvedName=UName "X"} I8 _loc1)
                       _loc2)
                      (TEnum Name {resolvedName=UName "Bar"} _loc3 False)
                    }
                  ]
                }
              , D_Enum Enum{enumName="Bar"}
              ]
            }
          ) -> return ()
    Right _ -> assertFailure "not equal"
    Left _ -> assertFailure "type error"

annotationTest :: Test
annotationTest = TestLabel "int annotaion" $ TestCase $ do
  let decls = [ mkStruct "Foo"
                [ mkField 1 "foo"
                  (AnnotatedType I64
                   (Just Annotations
                    { annList =
                       [ValueAnn
                        { vaTag = "hs.type"
                        , vaVal = TextAnn "Int" DoubleQuote
                        , vaTagLoc = nlc, vaEqual = nlc, vaValLoc = nlc
                        , vaSep = NoSep
                        }
                       ]
                    , annOpenParen = nlc, annCloseParen = nlc
                    })
                   (Arity0Loc nlc))
                ]
              ]
  case fst <$> typecheck (defaultOptions defaultHsOpts) (mkThrift decls) of
    Right ( Program
            { progDecls =
              [ D_Struct Struct
                { structResolvedName = "Foo"
                , structMembers =
                  [ Field
                    { fieldId = 1
                    , fieldName = "foo"
                    , fieldResolvedType = TSpecial HsInt
                    }
                  ]
                }
              ]
            }
          ) -> return ()
    _ -> assertFailure "not equal"

cycleTest :: Test
cycleTest = TestLabel "cyclic types" $ TestCase $ do
  let decls = [ mkTypedef "X" $ mkNamedType "Y"
              , mkTypedef "Y" $ mkNamedType "Z"
              , mkTypedef "Z" $ mkNamedType "X"
              ]
  let result = typecheck (defaultOptions NoOpts) (mkThrift decls)
  case fst <$> result of
    Left [TypeError _ (CyclicTypes _)] -> return ()
    _ -> assertFailure "Should have been a type error"

mkNamedType :: Text -> AnnotatedType Loc ()
mkNamedType name = AnnotatedType (TNamed name) Nothing (Arity0Loc nlc)

mkTypedef :: Text -> AnnotatedType Loc t -> Parsed Decl
mkTypedef name ty = D_Typedef Typedef
  { tdName = name
  , tdTag  = IsTypedef
  , tdType = ty
  , tdResolvedName = ()
  , tdResolvedType = ()
  , tdLoc = TypedefLoc nlc nlc
  , tdAnns = Nothing
  , tdSAnns = []
  }

unknownTest :: Test
unknownTest = TestLabel "unknown type" $ TestCase $ do
  let
    decls = [ mkStruct "Foo"
              [ mkField 1 "foo" $ mkNamedType "X"
              ]
            ]
    result = typecheck (defaultOptions NoOpts) (mkThrift decls)
  case result of
    Left [TypeError _ (UnknownType _)] -> return ()
    _ -> assertFailure "Should have been a type error"

mkStruct :: Text -> [Parsed (Field 'StructField)] -> Parsed Decl
mkStruct name fields = D_Struct Struct
  { structName = name
  , structType = StructTy
  , structMembers = fields
  , structResolvedName = ()
  , structLoc = StructLoc nlc nlc nlc nlc
  , structAnns = Nothing
  , structSAnns = []
  }

mkField
  :: Int32
  -> Text
  -> AnnotatedType Loc t
  -> Parsed (Field 'StructField)
mkField fid name ty = Field
  { fieldId = fid
  , fieldName = name
  , fieldResolvedName = ()
  , fieldRequiredness = Default
  , fieldType = ty
  , fieldResolvedType = ()
  , fieldVal = Nothing
  , fieldResolvedVal = ()
  , fieldLaziness = Lazy
  , fieldTag = STRUCT_FIELD
  , fieldLoc = FieldLoc nlc (showt fid) nlc nlc Nothing NoSep
  , fieldAnns = Nothing
  , fieldSAnns = []
  }

-- Constant Typechecking Tests -------------------------------------------------

simpleStruct :: String
simpleStruct = unlines
  [ "struct Foo {"
  , "  1: i32 x = 1,"
  , "  2: bool y = 0,"
  , "  3: string z = 'hello'"
  , "}"
  ]

complexTypes :: String
complexTypes = unlines
  [ "struct Foo {"
  , "  1: list<i32> x = [ 1, 2, 3],"
  , "  2: set<i32> y = [0],"
  , "  3: map<byte, i64> z = { 1 : 2, 2 : 3 }"
  , "}"
  ]

structConstant :: String
structConstant = unlines
  [ "struct Foo {"
  , "  1: Bar bar = { 'y' : 0, 'z' : '>>=' }"
  , "}"
  , ""
  , "struct Bar {"
  , "  1: optional i64 x,"
  , "  2: bool y,"
  , "  3: optional string z"
  , "}"
  ]

unionConsts :: String
unionConsts = unlines
  [ "# Lambda calculus in thrift"
  , ""
  , "union Exp {"
  , "  1: string EVar"
  , "  2: Lambda ELambda"
  , "  3: App EApp"
  , "}"
  , ""
  , "struct Lambda {"
  , "  1: string bind"
  , "  2: Exp body"
  , "} (hs.prefix = \"\")"
  , ""
  , "struct App {"
  , "  1: Exp e1"
  , "  2: Exp e2"
  , "} (hs.prefix = \"\")"
  , ""
  , "# identity function in thrift"
  , "const Exp identity = {"
  , "  \"ELambda\" : { \"bind\" : \"x\", \"body\" : { \"EVar\" : \"x\" } }"
  , "}"
  , ""
  , "# apply identity function to itelf"
  , "const Exp id_id = { \"EApp\" : { \"e1\" : identity, \"e2\" : identity } }"
  ]

typedefException :: String
typedefException = unlines
  [ "exception MyException { 1: string err }"
  , "typedef MyException MyTypedef"
  ]

typecheckTests :: Test
typecheckTests = TestList $ map (uncurry typecheckTest)
                 [ ( "simple struct", simpleStruct )
                 , ( "complex types", complexTypes )
                 , ( "struct consts", structConstant )
                 , ( "union consts", unionConsts )
                 , ( "typedef exception", typedefException )
                 ]

typecheckTest :: String -> String -> Test
typecheckTest label input = TestLabel label $ TestCase $
  case parseAndTypecheck NoOpts input of
    Right _ -> return ()
    Left _ -> assertFailure "should have typechecked"

simpleError :: String
simpleError = unlines
  [ "struct Foo {"
  , "  1: list<i32> foo = [ 1.5 ]"
  , "}"
  ]

i64IntError :: String
i64IntError = unlines
  [ "const i64 (hs.type = 'Int') intThing = 1"
  , "const i64 i64Thing = intThing"
  ]

newtypeError :: String
newtypeError = unlines
  [ "typedef i64 X (hs.newtype)"
  , "const i64 i = 0"
  , "const X x = i"
  ]

nestedNewtypeError :: String
nestedNewtypeError = unlines
  [ "typedef i64 X (hs.newtype)"
  , "const X x = 100"
  , "const list<i64> listThing = [x]"
  ]

vectorError :: String
vectorError = unlines
  [ "const list<i64> listThing = [1, 2, 3]"
  , "const list<i64> (hs.type = 'Vector') vectorThing = listThing"
  ]

zeroFieldError :: String
zeroFieldError = unlines
  [ "struct X {"
  , "  0: i64 x"
  , "}"
  ]

duplicateFieldError :: String
duplicateFieldError = unlines
  [ "struct X {"
  , "  1: i64 x"
  , "  1: i64 y"
  , "}"
  ]

throwsError :: String
throwsError = unlines
  [ "service X {"
  , "  void foo() throws (1: i64 x)"
  , "}"
  ]

emptyUnionError :: String
emptyUnionError = "union X {}"

duplicateEnumError :: String
duplicateEnumError = unlines
  [ "enum X {"
  , "  A = 1"
  , "  B = 1"
  , "}"
  ]

typeErrorTests :: Test
typeErrorTests = TestList $
  map (uncurry $ typeErrorTest NoOpts)
    [ ( "simple error", simpleError )
    , ( "FieldId = 0", zeroFieldError )
    , ( "duplicate FieldId", duplicateFieldError )
    , ( "throws error", throwsError )
    , ( "empty union", emptyUnionError )
    , ( "duplicate enum", duplicateEnumError )
    ] ++
  map (uncurry $ typeErrorTest defaultHsOpts)
    [ ( "i64 int error", i64IntError )
    , ( "Newtype Error", newtypeError )
    , ( "Nested Newtype Error", nestedNewtypeError )
    , ( "Vector Error", vectorError )
    ]

typeErrorTest :: Typecheckable l => LangOpts l -> String -> String -> Test
typeErrorTest opts label input = TestLabel label $ TestCase $
  case parseAndTypecheck opts input of
    Left errors | any isTypeError errors -> return ()
    _ -> assertFailure "should be a type mismatch"

isTypeError :: TypeError l -> Bool
isTypeError TypeError{} = True
isTypeError EmptyInput = False
isTypeError CyclicModules{} = False

-- Name Collision Tests --------------------------------------------------------

nameCollision :: String
nameCollision = unlines
  [ "struct X {"
  , "  1: i32 x"
  , "}"
  , "const i32 x_x = 100"
  ]

typeCollision :: String
typeCollision = unlines
  [ "struct X {"
  , "  1: i32 x"
  , "}"
  , "typedef i32 X"
  ]

enumHSNameCollision :: String
enumHSNameCollision = unlines
  [ "enum A { A = 0}"
  , "enum Y { A = 0} (hs.prefix = \"A_\")"
  ]

unionHSNameCollition :: String
unionHSNameCollition = unlines
  [ "union A {"
  , "  1: i64 Alt"
  , "}"
  , "union B {"
  , "  1: i64 t"
  , "} (hs.prefix = \"A_Al\")"
  ]

collisionTests :: Test
collisionTests = TestList $ map (uncurry collisionTest)
                 [ ( "name collision", nameCollision )
                 , ( "type collision", typeCollision )
                 , ( "enum haskell name collision", enumHSNameCollision)
                 , ( "union haskell name collision", unionHSNameCollition)
                 ]

collisionTest :: String -> String -> Test
collisionTest label input = TestLabel label $ TestCase $
  case parseAndTypecheck defaultHsOpts input of
    Left [TypeError _ DuplicateName{}] -> return ()
    _ -> assertFailure "should be a duplicate name error"

-- Multiple Errors -------------------------------------------------------------

multipleErrors :: String
multipleErrors = unlines
  [ "const i32 x = 'foo'"
  , "const i32 y = 'bar'"
  , "const i32 z = 'baz'"
  ]

multiErrorTest :: Test
multiErrorTest = TestLabel "multi error" $ TestCase $
  case parseAndTypecheck NoOpts multipleErrors of
    Left es -> assertEqual "3 errors" (length es) 3
    _ -> assertFailure "should have been a type error"

--------------------------------------------------------------------------------

main :: IO ()
main = testRunner $ TestList
  [ resolutionTests, typecheckTests, typeErrorTests
  , collisionTests, multiErrorTest ]
