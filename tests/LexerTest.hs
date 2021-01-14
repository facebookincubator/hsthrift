-- Copyright (c) Facebook, Inc. and its affiliates.

{-# OPTIONS_GHC -fno-warn-orphans #-}
module LexerTest where

import Control.Applicative
import qualified Data.Text as Text
import System.Exit
import Test.QuickCheck
import TextShow

import Thrift.Compiler.Lexer
import Thrift.Compiler.Types

instance Arbitrary Token where
  arbitrary = (\tok -> Tok tok nlc) <$> arbitrary

string :: Gen String
string = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

symbol :: Gen String
symbol = (:) <$> elements (['a'..'z'] ++ ['A'..'Z'])
             <*> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

instance Arbitrary TokenType where
    arbitrary = oneof [ pure L_CURLY_BRACE, pure R_CURLY_BRACE
                      , pure L_ANGLE_BRACE, pure R_ANGLE_BRACE
                      , pure L_SQUARE_BRACE, pure R_SQUARE_BRACE
                      , pure L_PAREN, pure R_PAREN
                      , pure COMMA, pure SEMICOLON, pure COLON, pure EQUALS
                      , pure INCLUDE, pure HS_INCLUDE
                      , pure STRUCT, pure ENUM, pure TYPEDEF, pure CONST
                      , pure REQUIRED, pure OPTIONAL
                      , pure MAP, pure LIST, pure SET
                      , pure INT8, pure INT16, pure INT32, pure INT64
                      , pure DOUBLE, pure FLOAT
                      , pure BOOL, pure STRING, pure VOID
                      , (\i -> INTEGRAL i $ showt i) <$> arbitrary
                      , (\f -> FLOATING f $ showt f) <$> arbitrary
                      , STRING_LIT <$> string <*> arbitrary
                      , SYMBOL <$> symbol
                      , pure TRUE, pure FALSE
                      ]

instance Arbitrary QuoteType where
  arbitrary = elements [SingleQuote, DoubleQuote]

ppT :: TokenType -> String
ppT L_CURLY_BRACE = "{"
ppT R_CURLY_BRACE = "}"
ppT L_ANGLE_BRACE = "<"
ppT R_ANGLE_BRACE = ">"
ppT L_SQUARE_BRACE = "["
ppT R_SQUARE_BRACE = "]"
ppT L_PAREN = "("
ppT R_PAREN = ")"
ppT COMMA = ","
ppT SEMICOLON = ";"
ppT COLON = ":"
ppT EQUALS = "="
ppT AT = "@"
ppT STRUCT = "struct"
ppT ENUM = "enum"
ppT TYPEDEF = "typedef"
ppT CONST = "const"
ppT REQUIRED = "required"
ppT OPTIONAL = "optional"
ppT MAP = "map"
ppT LIST = "list"
ppT SET = "set"
ppT INT8 = "byte"
ppT INT16 = "i16"
ppT INT32 = "i32"
ppT INT64 = "i64"
ppT DOUBLE = "double"
ppT FLOAT  = "float"
ppT BOOL = "bool"
ppT STRING = "string"
ppT (INTEGRAL _ r) = Text.unpack r
ppT (FLOATING _ r) = Text.unpack r
ppT (STRING_LIT s qt) = q ++ s ++ q
  where q = case qt of { SingleQuote -> "'" ; DoubleQuote -> "\"" }
ppT (SYMBOL s) = s
ppT NAMESPACE = "namespace"
ppT INCLUDE = "include"
ppT HS_INCLUDE = "hs_include"
ppT HASH_MAP = "hash_map"
ppT HASH_SET = "hash_set"
ppT BINARY = "binary"
ppT VOID = "void"
ppT UNION = "union"
ppT VIEW = "view"
ppT EXCEPTION = "exception"
ppT SERVICE = "service"
ppT ONEWAY = "oneway"
ppT EXTENDS = "extends"
ppT THROWS = "throws"
ppT ASYNC = "async"
ppT CPP_INCLUDE = "cpp_include"
ppT SENUM = "senum"
ppT STREAM = "stream"
ppT TRUE = "true"
ppT FALSE = "false"

pp :: [Token] -> String
pp tokens = unwords [ ppT tok | Tok tok _ <- tokens ]

prop_roundTrip :: [Token] -> Bool
prop_roundTrip =
  liftA2 (==) (Right . getTokens) (fmap getTokens . lexThrift . pp)
  where
    getTokens ts = [ t | Tok t _ <- ts ]

main :: IO ()
main = do
  result <- quickCheckResult prop_roundTrip
  case result of
    Success{} -> exitSuccess
    _         -> exitFailure
