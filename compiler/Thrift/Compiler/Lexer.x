-- Copyright (c) Facebook, Inc. and its affiliates.

{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Thrift.Compiler.Lexer
  ( lexThrift
  , Token(..), TokenType(..)
  , AlexPosn(..), Alex(..)
  , setFilename, getComments
  , alexMonadScan, alexGetInput, runAlex, alexError
  ) where

import Control.Monad
import Control.Monad.Trans.State
import Data.List.Extra
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readEither)

import Thrift.Compiler.Types
}

%wrapper "monadUserState"

$all        = [.\n]

$digit      = [0-9]
@decimal    = [\+\-]?$digit+
@floating   = [\+\-]?$digit* "." $digit+
@sciSuffix  = [eE][\+\-]?$digit+
@scientific = (@floating@sciSuffix | @decimal@sciSuffix)
@hex        = 0x[0-9a-fA-F]+

-- Note: Thrift strings don't support escaped characters :(
@string     = \' ($all # \')* \'
@dstring    = \" ($all # \")* \"

@notEnd     = ($all # [\*\/]) | ($all # \*)"/" | "*"($all # \/)
@comment    = "/*" @notEnd*"*"* "*/"

tokens :-
  $white+              ;
  -- Comments
  "#"[^\n]*            { addComment }
  "//"[^\n]*           { addComment }
  @comment             { addComment }

  -- Keywords
  "struct"             { basicToken STRUCT }
  "enum"               { basicToken ENUM }
  "typedef"            { basicToken TYPEDEF }
  "const"              { basicToken CONST }
  "required"           { basicToken REQUIRED }
  "optional"           { basicToken OPTIONAL }
  "map"                { basicToken MAP }
  "list"               { basicToken LIST }
  "set"                { basicToken SET }
  "byte"               { basicToken INT8 }
  "i16"                { basicToken INT16 }
  "i32"                { basicToken INT32 }
  "i64"                { basicToken INT64 }
  "double"             { basicToken DOUBLE }
  "float"              { basicToken FLOAT }
  "bool"               { basicToken BOOL }
  "string"             { basicToken STRING }
  "void"               { basicToken VOID }
  "true"               { basicToken TRUE }
  "false"              { basicToken FALSE }
  "namespace"          { basicToken NAMESPACE }
  "include"            { basicToken INCLUDE }
  "hs_include"         { basicToken HS_INCLUDE }
  "hash_map"           { basicToken HASH_MAP }
  "hash_set"           { basicToken HASH_SET }
  "binary"             { basicToken BINARY }
  "senum"              { basicToken SENUM }
  "stream"             { basicToken STREAM }
  "void"               { basicToken VOID }
  "union"              { basicToken UNION }
  "view"               { basicToken VIEW }
  "exception"          { basicToken EXCEPTION }
  "service"            { basicToken SERVICE }
  "oneway"             { basicToken ONEWAY }
  "extends"            { basicToken EXTENDS }
  "throws"             { basicToken THROWS }
  "async"              { basicToken ASYNC }
  "cpp_include"        { basicToken CPP_INCLUDE }


  -- Literals
  @scientific          { numToken FLOATING }
  @floating            { numToken FLOATING }
  @decimal             { numToken INTEGRAL }
  @hex                 { numToken INTEGRAL }
  @string|@dstring     { strToken }

  -- Identifiers
 [a-zA-Z_] [a-zA-Z0-9_\.]*  { symbolToken }

  -- Symbols
  "{"                  { basicToken L_CURLY_BRACE }
  "}"                  { basicToken R_CURLY_BRACE }
  "["                  { basicToken L_SQUARE_BRACE }
  "]"                  { basicToken R_SQUARE_BRACE }
  "<"                  { basicToken L_ANGLE_BRACE }
  ">"                  { basicToken R_ANGLE_BRACE }
  "("                  { basicToken L_PAREN }
  ")"                  { basicToken R_PAREN }
  ","                  { basicToken COMMA }
  ":"                  { basicToken COLON }
  ";"                  { basicToken SEMICOLON }
  "="                  { basicToken EQUALS }
  "@"                  { basicToken AT }

{

data Token
  = Tok TokenType (Located Loc)
  | EOF
  deriving (Show, Eq)

data TokenType
  -- Braces
  = L_CURLY_BRACE  | R_CURLY_BRACE
  | L_ANGLE_BRACE  | R_ANGLE_BRACE
  | L_SQUARE_BRACE | R_SQUARE_BRACE
  | L_PAREN        | R_PAREN
  -- Puntuation
  | COMMA | SEMICOLON | COLON | EQUALS | AT
  -- Headers
  | NAMESPACE | INCLUDE | HS_INCLUDE
  -- Other Keywords
  | STRUCT | ENUM | TYPEDEF | CONST
  | REQUIRED | OPTIONAL
  -- Types
  | MAP | LIST | SET
  | HASH_MAP | HASH_SET
  | INT8 | INT16 | INT32 | INT64
  | DOUBLE | FLOAT
  | BOOL | STRING
  -- Literals
  | INTEGRAL Int Text
  | FLOATING Double Text
  | STRING_LIT String QuoteType
  | SYMBOL String
  | TRUE | FALSE
  -- Stuff we don't use (yet)
  | SENUM | STREAM | VOID | BINARY
  | UNION | VIEW | EXCEPTION | SERVICE
  | ONEWAY | EXTENDS | THROWS | ASYNC
  -- Stuff we won't use ever
  | CPP_INCLUDE
  deriving (Show, Eq)

-- Note: the following tokens exist, but as far as I can tell aren't ever used
-- We exclude them for brevity:
--
-- java_package, cocoa_prefix, csharp_namespace, php_namespace, ruby_namespace,
-- py_module, perl_package, smalltalk_category, smalltalk_prefix, xsd_all,
-- xsd_optional, xsd_nillable, xsd_namespace, xsd_attrs, slist

data AlexUserState = AlexUserState
  { ust_filename :: FilePath
  , ust_comments :: [Comment Loc] -> [Comment Loc]
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "" id

getFilename :: Alex FilePath
getFilename = Alex $ \s@AlexState{alex_ust=AlexUserState{..}} ->
  Right (s, ust_filename)

setFilename :: FilePath -> Alex ()
setFilename f = Alex $ \s@AlexState{..} ->
  Right (s {alex_ust = alex_ust { ust_filename = f } }, ())

getComments :: Alex [Comment Loc]
getComments = Alex $ \s@AlexState{alex_ust=ust@AlexUserState{..}} ->
  Right (s { alex_ust = ust { ust_comments = id } }, ust_comments [])

addComment :: AlexInput -> Int -> Alex Token
addComment i@(_,_,_,input) len = do
  loc <- getLoc i len
  let c = Comment loc $ Text.pack $ take len input
  Alex $ \s@AlexState{alex_ust=ust@AlexUserState{..}} ->
    Right (s{alex_ust = ust{ust_comments = ust_comments . (c:)}}, ())
  alexMonadScan

getLoc :: AlexInput -> Int -> Alex Loc
getLoc (AlexPn _ l c,_,_,input) len = do
  file <- getFilename
  let
    chunk = take len input
    newlines = length $ filter (== '\n') chunk
    endcol | newlines == 0 = c + len
           | otherwise = length $ takeWhileEnd (/= '\n') chunk
  return $ Loc file l c (l + newlines) endcol

basicToken :: TokenType -> AlexInput -> Int -> Alex Token
basicToken tok i@(AlexPn _ l c,_,_,_) len = do
  loc <- getLoc i len
  cs <- getComments
  pure $ Tok tok $ Located cs loc

strToken :: AlexInput -> Int -> Alex Token
strToken i@(_, _, _, input) len = basicToken (mkString str qtype) i len
  where
    str = take len input
    mkString s = STRING_LIT $ drop 1 $ take (n - 1) s
      where n = length s
    qtype = case str of
      '"':_ -> DoubleQuote
      _     -> SingleQuote

symbolToken :: AlexInput -> Int -> Alex Token
symbolToken i@(p, _, _, input) len = basicToken (SYMBOL (take len input)) i len

numToken
  :: (Read a, Num a)
  => (a -> Text -> TokenType)
  -> AlexInput
  -> Int
  -> Alex Token
numToken mkTok i@(AlexPn _ l c, _, _, input) len =
  case parseNumWithSign rawInput of
    Left err -> do
      file <- getFilename
      alexError $ concat
        [file, ":", show l, ":", show c, ": ", err]
    Right num -> basicToken (mkTok num $ Text.pack rawInput) i len
  where
    rawInput = take len input

    parseNumWithSign ('+' : num) = parseNum num
    parseNumWithSign ('-' : num) = negate <$> parseNum num
    parseNumWithSign num = parseNum num

    -- Read doesn't like numbers starting with decimal points, so concat a `0`
    -- to the front if it isn't there.
    parseNum num@('.':_) = readEither $ '0':num
    parseNum num = readEither num

alexEOF :: Alex Token
alexEOF = pure EOF

lexThrift :: String -> Either String [Token]
lexThrift input = runAlex input go
  where
    go = do
      tok <- alexMonadScan
      case tok of
        EOF -> return []
        _ -> (tok :) <$> go

}
