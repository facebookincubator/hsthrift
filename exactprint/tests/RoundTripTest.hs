module RoundTripTest (main) where

import qualified Data.Text.Lazy as Text
import Test.HUnit
import TestRunner

import Thrift.Compiler.Parser
import Thrift.ExactPrint.Convert
import Thrift.ExactPrint.PrettyPrint

structInput :: String
structInput = unlines
  [ "// This is my struct"
  , "struct X /* hi */   {"
  , "  0x1: required bool x = true;"
  , "  2: i64 (hs.type = \"Int\") y"
  , "  // this is a map"
  , "  3: map <  string,byte  > z = { \"s\" : 255 }"
  , "  }"
  , ""
  , ""
  , "struct Y { 1: list<i64> ints = [1,  2; 3"
  , "4 5 /* the next number is 6 */ 6"
  , "// there's still more"
  , "7"
  , "]}"
  ]

enumInput :: String
enumInput = unlines
  [ "enum X {A=1,   B  =2; C = 003 }"
  , "enum Y {"
  , "  // Comment"
  , "  A = 0,"
  , "  B = 0x123 (cpp.name = \"XXX\", hs.prefix = \"X\"),"
  , "}"
  ]

constInput :: String
constInput = unlines
  [ "const i64 a = /* three */ 0x3"
  , "const string b = 'B' ; // comment"
  , "const list <bool> c = [true 1; false, 0]"
  , "const double d = 2e-7;"
  , "const double e = 1.0001"
  ]

unionInput :: String
unionInput = unlines
  [ "union X { 1: i64 a"
  , "        , 2: string b"
  , "        , 3: map< string, /* this */ X>  c"
  , "        }"
  ]

serviceInput :: String
serviceInput = unlines
  [ "service X {"
  , " void foo() (prority = \"LOW\");"
  , " oneway  string bar(1: bool x,"
  , "    2:  i32 y,);  }"
  , "exception EX {}"
  , "service Y   extends    X    {"
  , "  list<i16> foo(1:  string x  ) throws (1: X ex);"
  , "}"
  ]

cppIncludeInput :: String
cppIncludeInput = unlines
  [ "cpp_include \"<unordered_set>\""
  , "cpp_include \"common/hash/Hash.h\""
  , "cpp_include \"<unordered_set>\" // comment"
  , "cpp_include /* comment */ \"common/hash/Hash.h\""
  ]

listInput :: String
listInput = "const list<i64> list_value = [0, 1]\n"

mapInput :: String
mapInput = "const map<i64, bool> map_value = { 0: true, 1: false }\n"

roundTripTest :: String -> String -> Test
roundTripTest label input = TestLabel ("Round Trip Raw " ++ label) $ TestCase
  $ do
    case runParser parseThrift label input of
      Left e -> fail e
      Right parsedData -> assertEqual "roundtrip raw" input $ Text.unpack $
        exactPrintThrift $ computeThriftFileOffsets $ mkThriftFile parsedData
  where
    mkThriftFile (headers, decls) = ThriftFile
      { thriftName    = ""
      , thriftPath    = ""
      , thriftHeaders = headers
      , thriftDecls   = decls
      , thriftSplice  = ()
      , thriftComments = []
      }

main :: IO ()
main = testRunner $ TestList
  [ roundTripTest "Struct" structInput
  , roundTripTest "Enum" enumInput
  , roundTripTest "Const" constInput
  , roundTripTest "Union" unionInput
  , roundTripTest "Service" serviceInput
  , roundTripTest "CppInclude" cppIncludeInput
  , roundTripTest "List" listInput
  , roundTripTest "Map" mapInput
  ]
