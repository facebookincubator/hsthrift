module InteractionsTest where

import Test.HUnit
import TestRunner

import Thrift.Compiler.Parser

srcFile :: String
srcFile = unlines
  [ "interaction MyInteraction {"
  , "  void foo();"
  , "  i64 bar();"
  , "}"
  ]

interactionTest :: Test
interactionTest = TestLabel "Interaction Test" $ TestCase $ do
  let file = "foo_bar"
  case snd <$> runParser parseThrift (file ++ ".thrift") srcFile of
    Right [ D_Interaction {}] -> return ()
    _ -> assertFailure "not equal"

main :: IO ()
main = testRunner interactionTest
