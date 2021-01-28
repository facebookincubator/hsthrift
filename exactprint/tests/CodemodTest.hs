-- Copyright (c) Facebook, Inc. and its affiliates.

module CodemodTest (main) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Test.HUnit
import TestRunner

import Thrift.Compiler.Types
import Thrift.ExactPrint.Codemod

-- Codemod Bools ---------------------------------------------------------------

boolsInput :: String
boolsInput = unlines
  [ "namespace hs Foo.Bar"
  , "namespace cpp 'Foo.Bar'"
  , "namespace py \"Foo.Bar\""
  , "struct X {"
  , "  1: B x = { \"b\" : 1 },"
  , "  2: list< bool> y = [0, 0  ,1, 0 ];"
  , "  3: map<i64, bool> z = {"
  , "    100 : 0,"
  , "    300 : 1,"
  , "  }"
  , "}"
  , "struct B { 1: bool b = 0 }"
  , "// End of module"
  , "  /* That's it */"
  ]

boolsOutput :: String
boolsOutput = unlines
  [ "namespace hs Foo.Bar"
  , "namespace cpp 'Foo.Bar'"
  , "namespace py \"Foo.Bar\""
  , "struct X {"
  , "  1: B x = { \"b\" : true },"
  , "  2: list< bool> y = [false, false  ,true, false ];"
  , "  3: map<i64, bool> z = {"
  , "    100 : false,"
  , "    300 : true,"
  , "  }"
  , "}"
  , "struct B { 1: bool b = false }"
  ]

convertBools :: Type l t -> ConstVal a -> ConstVal a
convertBools TBool (IntConst 0 _) = BoolConst False
convertBools TBool (IntConst 1 _) = BoolConst True
convertBools _ c = c

convertBoolsTest :: Test
convertBoolsTest = mkTest "Convert Bools" boolsInput boolsOutput $
  const convertBools

-- Qualify Enums ---------------------------------------------------------------

qenumsInput :: String
qenumsInput = unlines
  [ "enum X { A = 0; B = 1; C = 2 }"
  , "const X x = A"
  , "const X y = x"
  ]

qenumsOutput :: String
qenumsOutput = unlines
  [ "enum X { A = 0; B = 1; C = 2 }"
  , "const X x = X.A" -- This gets qualified
  , "const X y = x"   -- This does not
  ]

qualifyEnums :: Env l -> Type l t -> ConstVal a -> ConstVal a
qualifyEnums env (TEnum enum _loc1 _) (IdConst name)
  | Just (_, nameMap) <- lookupEnum enum env
  , Map.member name nameMap = qualifyEnum enum name
qualifyEnums _ _ c = c

qualifyEnum :: Name -> Text -> ConstVal a
qualifyEnum Name{..} name = IdConst $ Text.intercalate "." elems
  where
    elems = case sourceName of
      UName n -> [n, name]
      QName m n -> [m, n, name]

qualifyEnumsTest :: Test
qualifyEnumsTest = mkTest "Qualify Enums" qenumsInput qenumsOutput qualifyEnums

-- Convert Enums ---------------------------------------------------------------

convertEnumsInput :: String
convertEnumsInput = unlines
  [ "enum X { A = 0; B = 1; C = 2 }"
  , "const X x = 0"
  , "const X y = 2"
  ]

convertEnumsOutput :: String
convertEnumsOutput = unlines
  [ "enum X { A = 0; B = 1; C = 2 }"
  , "const X x = X.A"
  , "const X y = X.C"
  ]

convertEnums :: Env l -> Type l t -> ConstVal a -> ConstVal a
convertEnums env (TEnum enum _loc _) (IntConst x _)
  | Just (idMap, _) <- lookupEnum enum env
  , Just (Name{..}, _loc) <- Map.lookup (fromIntegral x) idMap =
    qualifyEnum enum (localName sourceName)
convertEnums _ _ c = c

convertEnumsTest :: Test
convertEnumsTest = mkTest "ConvertEnums" convertEnumsInput convertEnumsOutput
  convertEnums

-- -----------------------------------------------------------------------------

mkTest
  :: String
  -> String
  -> String
  -> (forall t l a. Env l -> Type l t -> ConstVal a -> ConstVal a)
  -> Test
mkTest label input output f = TestLabel label $ TestCase $
  assertEqual "exactprint" output $ roundTripWith (codemodConsts f) input

main :: IO ()
main = testRunner $ TestList
  [ convertBoolsTest
  , qualifyEnumsTest
  , convertEnumsTest
  ]
