-- Copyright (c) Facebook, Inc. and its affiliates.

{

#if __GLASGOW_HASKELL__ > 804
#define This Some
#endif

module Thrift.Compiler.Parser
  ( parseThrift, runParser
  , parse, parseString
  , ThriftFile(..), Header(..), Decl(..), getModuleName
  ) where

import Prelude hiding (Enum)
import Data.Bifunctor
import Data.Maybe
import Data.Some
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath

import Thrift.Compiler.Lexer
import Thrift.Compiler.Options
import Thrift.Compiler.Types

}

%name parseThrift
%tokentype { Token }
%lexer { lexWrap } { EOF }
%monad { Parser } { bind } { return }

%error { parseError }

%token struct      { Tok STRUCT _ }
       typedef     { Tok TYPEDEF _ }
       enum        { Tok ENUM _ }
       const       { Tok CONST _ }
       required    { Tok REQUIRED _ }
       optional    { Tok OPTIONAL _ }
       map         { Tok MAP _ }
       list        { Tok LIST _ }
       set         { Tok SET _ }
       byte        { Tok INT8 _ }
       i16         { Tok INT16 _ }
       i32         { Tok INT32 _ }
       i64         { Tok INT64 _ }
       double      { Tok DOUBLE _ }
       float       { Tok FLOAT _ }
       bool        { Tok BOOL _ }
       true        { Tok TRUE _ }
       false       { Tok FALSE _ }
       string      { Tok STRING _ }
       namespace   { Tok NAMESPACE _ }
       include     { Tok INCLUDE _ }
       hs_include  { Tok HS_INCLUDE _ }
       cpp_include { Tok CPP_INCLUDE _ }
       hash_map    { Tok HASH_MAP _ }
       hash_set    { Tok HASH_SET _ }
       intTok      { Tok INTEGRAL{} _ }
       doubleTok   { Tok FLOATING{} _ }
       stringTok   { Tok STRING_LIT{} _ }
       symTok      { Tok SYMBOL{} _ }
       '{'         { Tok L_CURLY_BRACE _ }
       '}'         { Tok R_CURLY_BRACE _ }
       '['         { Tok L_SQUARE_BRACE _ }
       ']'         { Tok R_SQUARE_BRACE _ }
       '<'         { Tok L_ANGLE_BRACE _ }
       '>'         { Tok R_ANGLE_BRACE _ }
       '('         { Tok L_PAREN _ }
       ')'         { Tok R_PAREN _ }
       ','         { Tok COMMA _ }
       ';'         { Tok SEMICOLON _ }
       ':'         { Tok COLON _ }
       '='         { Tok EQUALS _ }
       '@'         { Tok AT _ }
       -- Tokens for unused syntax
       binary    { Tok BINARY _ }
       senum     { Tok SENUM _ }
       stream    { Tok STREAM _ }
       void      { Tok VOID _ }
       union     { Tok UNION _ }
       view      { Tok VIEW _ }
       exception { Tok EXCEPTION _ }
       service   { Tok SERVICE _ }
       oneway    { Tok ONEWAY _ }
       extends   { Tok EXTENDS _ }
       throws    { Tok THROWS _ }

-- Shift/reduce conflicts
-- - UntypedConst needs to parse both an identifier and a struct which begins with an identifier
%expect 1

%%
Thrift :: { ([Header Loc], [Parsed Decl]) }
Thrift : list(Header) list(Decl) { (catMaybes $1, catMaybes $2) }

Header :: { Maybe (Header Loc) }
  : Include stringLit
    { Just HInclude
      { incPath = Text.unpack (lParsed $2)
      , incType = lVal $1
      , incKeywordLoc = lLoc $1
      , incPathLoc    = lLoc $2
      , incQuoteType  = lRep $2
      }
    }
  | namespace Symbol SymbolOrString
    { Just HNamespace
      { nmLang = lVal $2
      , nmName = lParsed $3
      , nmKeywordLoc = getLoc $1
      , nmLangLoc    = lLoc $2
      , nmNameLoc    = lLoc $3
      , nmQuoteType  = lRep $3
      }
    }

Include
  : include     { L (getLoc $1) Include }
  | hs_include  { L (getLoc $1) HsInclude }
  | cpp_include { L (getLoc $1) CppInclude }

SymbolOrString
  : stringLit { fmap (second Just) $1 }
  | Symbol    { fmap (,Nothing) $1 }

Decl :: { Maybe (Parsed Decl) }
  : Struct  { Just $ D_Struct $1 }
  | Union   { Just $ D_Union $1 }
  | Typedef { Just $ D_Typedef $1 }
  | Enum    { Just $ D_Enum $1 }
  | Const   { Just $ D_Const $1 }
  | Service { Just $ D_Service $1 }
  | UnusedDecl { Nothing }

-- Structs ---------------------------------------------------------------------

Struct :: { Parsed Struct }
Struct
  : StructuredAnnotations StructType Symbol '{' list(Field) '}' Annotations
    { Struct
      { structName         = lVal $3
      , structResolvedName = ()
      , structType         = lVal $2
      , structMembers      = $5
      , structLoc          = StructLoc
        { slKeyword    = lLoc $2
        , slName       = lLoc $3
        , slOpenBrace  = getLoc $4
        , slCloseBrace = getLoc $6
        }
      , structAnns         = $7
      , structSAnns        = $1
      }
    }

StructType
  : struct    { L (getLoc $1) StructTy }
  | exception { L (getLoc $1) ExceptionTy }

Field :: { Parsed (Field 'StructField) }
Field : StructuredAnnotations intLit ':' Req AnnotatedType Symbol MaybeConst Annotations Separator
        { case $5 of
            This t -> Field
              { fieldId           = fromIntegral $ lParsed $2
              , fieldName         = lVal $6
              , fieldResolvedName = ()
              , fieldType         = t
              , fieldResolvedType = ()
              , fieldVal          = fmap lVal $7
              , fieldResolvedVal  = ()
              , fieldRequiredness = $4
              , fieldLaziness     = Lazy
              , fieldTag          = STRUCT_FIELD
              , fieldLoc          = FieldLoc
                { flId        = lLoc $2
                , flIdRep     = lRep $2
                , flColon     = getLoc $3
                , flName      = lLoc $6
                , flEqual     = fmap lLoc $7
                , flSeparator = $9
                }
              , fieldAnns         = $8
              , fieldSAnns        = $1
              }
        }

Req :: { Requiredness 'StructField Loc }
Req : {- empty -}    { Default }
    | optional       { Optional $ getLoc $1 }
    | required       { Required $ getLoc $1 }

MaybeConst : {- empty -}      { Nothing }
           | '=' UntypedConst { Just $ L (getLoc $1) $2 }

-- Unions ----------------------------------------------------------------------

Union :: { Parsed Union }
Union
  : StructuredAnnotations union Symbol '{' list(UnionAlt) '}' Annotations
    { Union
      { unionName         = lVal $3
      , unionResolvedName = ()
      , unionAlts         = $5
      , unionEmptyName    = ()
      , unionHasEmpty     = HasEmpty
      , unionLoc          = StructLoc
        { slKeyword    = getLoc $2
        , slName       = lLoc $3
        , slOpenBrace  = getLoc $4
        , slCloseBrace = getLoc $6
        }
      , unionAnns         = $7
      , unionSAnns        = $1
      }
    }

UnionAlt :: { Parsed UnionAlt }
UnionAlt
  : StructuredAnnotations intLit ':' AnnotatedType Symbol MaybeConst Annotations Separator
    { case $4 of
        This t -> UnionAlt
          { altId           = fromIntegral $ lParsed $2
          , altName         = lVal $5
          , altResolvedName = ()
          , altType         = t
          , altResolvedType = ()
          , altLoc          = FieldLoc
            { flId        = lLoc $2
            , flIdRep     = lRep $2
            , flColon     = getLoc $3
            , flName      = lLoc $5
            , flEqual     = fmap lLoc $6
            , flSeparator = $8
            }
          , altAnns         = $7
          , altSAnns        = $1
          }
    }

-- Typedefs --------------------------------------------------------------------

Typedef :: { Parsed Typedef }
Typedef : StructuredAnnotations typedef AnnotatedType Symbol Annotations
          { case $3 of
              This t -> Typedef
                { tdName         = lVal $4
                , tdResolvedName = ()
                , tdTag          = IsTypedef
                , tdType         = t
                , tdResolvedType = ()
                , tdLoc          = TypedefLoc
                  { tdlKeyword = getLoc $2
                  , tdlName    = lLoc $4
                  }
                , tdAnns         = $5
                , tdSAnns        = $1
                }
          }

-- Annotations -----------------------------------------------------------------

Annotations :: { Maybe (Annotations Loc) }
Annotations
  : '(' list(Annotation) ')'
     { Just Annotations
       { annList = $2
       , annOpenParen  = getLoc $1
       , annCloseParen = getLoc $3
       }
     }
  | {- empty -} { Nothing }

Annotation :: { Annotation Loc }
Annotation
  : Symbol Separator
    { SimpleAnn
      { saTag = lVal $1
      , saLoc = lLoc $1
      , saSep = $2
      }
    }
  | Symbol '=' AnnValue Separator
    { ValueAnn
      { vaTag = lVal $1
      , vaVal = lVal $3
      , vaTagLoc = lLoc $1
      , vaEqual  = getLoc $2
      , vaValLoc = lLoc $3
      , vaSep    = $4
      }
    }

AnnValue
  : intLit    { fmap (uncurry IntAnn) $1 }
  | stringLit { fmap (uncurry TextAnn) $1 }

StructuredAnnotations : list(StructuredAnnotation) { $1 }

StructuredAnnotation
  : '@' Symbol
    { StructuredAnn
      { saAt = getLoc $1
      , saType = lVal $2
      , saTypeLoc = Arity0Loc $ lLoc $2
      , saMaybeElems = Nothing
      }
    }
  | '@' Symbol '{' list(StructElem) '}'
    { StructuredAnn
      { saAt = getLoc $1
      , saType = lVal $2
      , saTypeLoc = Arity0Loc $ lLoc $2
      , saMaybeElems = Just $ StructuredAnnElems
        { saOpenBrace = getLoc $3
        , saElems = $4
        , saCloseBrace = getLoc $5
        }
      }
    }

-- Constants -------------------------------------------------------------------

Const :: { Parsed Const }
Const : StructuredAnnotations const AnnotatedType Symbol '=' UntypedConst Separator
        { case $3 of
            This ty -> Const
              { constName         = lVal $4
              , constResolvedName = ()
              , constType         = ty
              , constResolvedType = ()
              , constVal          = $6
              , constResolvedVal  = ()
              , constLoc          = ConstLoc
                { clKeyword   = getLoc $2
                , clName      = lLoc $4
                , clEqual     = getLoc $5
                , clSeparator = $7
                }
              , constSAnns = $1
              }
        }

UntypedConst :: { UntypedConst Loc }
UntypedConst
  : intLit     { UntypedConst (lLoc $1) (uncurry IntConst $ lVal $1) }
  | doubleLit  { UntypedConst (lLoc $1) (uncurry DoubleConst $ lVal $1) }
  | stringLit  { UntypedConst (lLoc $1) (uncurry StringConst $ lVal $1) }
  | true       { UntypedConst (getLoc $1) (BoolConst True) }
  | false      { UntypedConst (getLoc $1) (BoolConst False) }
  | Symbol     { UntypedConst (lLoc $1) (IdConst $ lVal $1) }
  | '[' list(ListElem) ']'
    { UntypedConst
      { ucLoc   = getLoc $1
      , ucConst = ListConst
        { lvElems = $2
        , lvCloseBrace = getLoc $3
        }
      }
    }
  | '{' list(MapElem) '}'
    { UntypedConst
      { ucLoc   = getLoc $1
      , ucConst = MapConst
        { mvElems = $2
        , mvCloseBrace = getLoc $3
        }
      }
    }
  | Symbol '{' list(StructElem) '}'
    { UntypedConst
      { ucLoc   = lLoc $1
      , ucConst = StructConst
        { svType = lVal $1
        , svOpenBrace = getLoc $2
        , svElems = $3
        , svCloseBrace = getLoc $4
        }
      }
    }
ListElem : UntypedConst Separator { ListElem $1 $2 }

MapElem : UntypedConst ':' UntypedConst Separator
          { ListElem (MapPair $1 (getLoc $2) $3) $4 }

StructElem : Symbol '=' UntypedConst Separator
             { ListElem (StructPair (lVal $1) (lLoc $1) (getLoc $2) $3) $4 }

-- Enum Values -----------------------------------------------------------------

Enum :: { Parsed Enum }
Enum : StructuredAnnotations enum Symbol '{' list(EnumVal) '}' Annotations
       { Enum
         { enumName         = lVal $3
         , enumResolvedName = ()
         , enumIsPseudo     = ()
         , enumConstants    = $5
         , enumLoc          = StructLoc
           { slKeyword    = getLoc $2
           , slName       = lLoc $3
           , slOpenBrace  = getLoc $4
           , slCloseBrace = getLoc $6
           }
         , enumAnns = $7
         , enumSAnns = $1
         , enumNoUnknown = ()
         }
       }

EnumVal :: { Parsed EnumValue }
EnumVal : StructuredAnnotations Symbol '=' intLit Annotations Separator
          { EnumValue
            { evName         = lVal $2
            , evResolvedName = ()
            , evValue        = fromIntegral $ lParsed $4
            , evLoc          = EnumValLoc
              { evlName      = lLoc $2
              , evlEqual     = getLoc $3
              , evlValue     = lLoc $4
              , evlRep       = lRep $4
              , evlSeparator = $6
              }
            , evAnns = $5
            , evSAnns = $1
            }
          }

-- Services --------------------------------------------------------------------

Service :: { Parsed Service }
Service
  : StructuredAnnotations service Symbol Extends '{' list(Function) '}' Annotations
     { Service
       { serviceName         = lVal $3
       , serviceResolvedName = ()
       , serviceSuper        = $4
       , serviceFunctions    = $6
       , serviceLoc          = StructLoc
         { slKeyword    = getLoc $2
         , slName       = lLoc $3
         , slOpenBrace  = getLoc $5
         , slCloseBrace = getLoc $7
         }
       , serviceAnns         = $8
       , serviceSAnns        = $1
       }
     }

Function :: { Parsed Function }
  : StructuredAnnotations IsOneway FunType Symbol '(' list(Argument) ')' Throws Annotations Separator
    { Function
      { funName         = lVal $4
      , funResolvedName = ()
      , funType         = $3
      , funResolvedType = ()
      , funArgs         = $6
      , funExceptions   = maybe [] snd $8
      , funIsOneWay     = isJust $2
      , funPriority     = NormalPriority
      , funLoc          = FunLoc
        { fnlOneway     = $2
        , fnlName       = lLoc $4
        , fnlOpenParen  = getLoc $5
        , fnlCloseParen = getLoc $7
        , fnlThrows     = fmap fst $8
        , fnlSeparator  = $10
        }
      , funAnns         = $9
      , funSAnns        = $1
      }
    }

Argument :: { Parsed (Field 'Argument) }
Argument : StructuredAnnotations intLit ':' AnnotatedType Symbol MaybeConst Annotations Separator
  { case $4 of
     This t -> Field
       { fieldId           = fromIntegral $ lParsed $2
       , fieldName         = lVal $5
       , fieldResolvedName = ()
       , fieldType         = t
       , fieldResolvedType = ()
       , fieldVal          = fmap lVal $6
       , fieldResolvedVal  = ()
       , fieldRequiredness = Default
       , fieldLaziness     = Lazy
       , fieldTag          = ARGUMENT
       , fieldLoc          = FieldLoc
         { flId        = lLoc $2
         , flIdRep     = lRep $2
         , flColon     = getLoc $3
         , flName      = lLoc $5
         , flEqual     = fmap lLoc $6
         , flSeparator = $8
         }
       , fieldAnns         = $7
       , fieldSAnns        = $1
       }
  }

Extends
  : extends Symbol
    { Just Super
        { supName         = lVal $2
        , supResolvedName = ()
        , supExtends      = getLoc $1
        , supLoc          = lLoc $2
        }
    }
  | {- nothing -}  { Nothing }

IsOneway
  : oneway      { Just $ getLoc $1  }
  | {- empty -} { Nothing }

Throws
  : throws '(' list(Throw) ')'
    { Just
      (ThrowsLoc
        { tlThrows     = getLoc $1
        , tlOpenParen  = getLoc $2
        , tlCloseParen = getLoc $4
        },
       $3)
    }
  | {- empty -} { Nothing }

FunType
  : AnnotatedType { Right $1 }
  | void          { Left $ getLoc $1 }

Throw :: { Parsed (Field 'Throws) }
Throw : StructuredAnnotations intLit ':' AnnotatedType Symbol MaybeConst Annotations Separator
        { case $4 of
            This t -> Field
              { fieldId           = fromIntegral $ lParsed $2
              , fieldName         = lVal $5
              , fieldResolvedName = ()
              , fieldType         = t
              , fieldResolvedType = ()
              , fieldVal          = fmap lVal $6
              , fieldResolvedVal  = ()
              , fieldRequiredness = Default
              , fieldLaziness     = Lazy
              , fieldTag          = THROWS_UNRESOLVED
              , fieldLoc          = FieldLoc
                { flId        = lLoc $2
                , flIdRep     = lRep $2
                , flColon     = getLoc $3
                , flName      = lLoc $5
                , flEqual     = fmap lLoc $6
                , flSeparator = $8
                }
              , fieldAnns         = $7
              , fieldSAnns        = $1
              }
        }


-- Unused Stuff ----------------------------------------------------------------

-- We don't use any of this syntax at the moment, but we need to be aware of it
-- for compatibility

UnusedDecl
  : view Symbol ':' Symbol '{' list(ViewField) '}' Annotations  {}
  | senum Symbol '{' list(stringLit) '}'                        {}

ViewField
  : Symbol Annotations                              {}
  | intLit ':' Req AnnotatedType Symbol Annotations {}

-- Other------------------------------------------------------------------------

list(p) : revlist(p) { reverse $1 }

revlist(p) : revlist(p) p { $2 : $1 }
           | {- empty -}  { [] }

Symbol :: { L Text }
  : symTok
    {% case $1 of
        Tok (SYMBOL s) loc -> return $ L loc $ Text.pack s
        _ -> parseError $1
    }
  -- view isn't actually a keyword according to the fbthrift lexer
  -- even though it probably should be
  | view { L (getLoc $1) "view" }

stringLit : stringTok
  {% case $1 of
       Tok (STRING_LIT s qt) loc -> return $ L loc (Text.pack s, qt)
       _ -> parseError $1
  }

intLit : intTok
  {% case $1 of
       Tok (INTEGRAL x s) loc -> return $ L loc (x, s)
       _ -> parseError $1
  }

doubleLit : doubleTok
  {% case $1 of
       Tok (FLOATING x s) loc -> return $ L loc (x, s)
       _ -> parseError $1
  }

Separator :: { Separator Loc }
  : ',' { Comma     $ getLoc $1 }
  | ';' { Semicolon $ getLoc $1 }
  |     { NoSep }

AnnotatedType :: { Some (AnnotatedType Loc) }
AnnotatedType : Type Annotations
                { case $1 of
                    ThisAnnTy ty loc -> This $ AnnotatedType ty $2 loc
                }

Type :: { SomeAnnTy 'Unresolved () }
Type : byte   { ThisAnnTy I8 (annTy0 $1) }
     | i16    { ThisAnnTy I16 (annTy0 $1) }
     | i32    { ThisAnnTy I32 (annTy0 $1) }
     | i64    { ThisAnnTy I64 (annTy0 $1) }
     | double { ThisAnnTy TDouble (annTy0 $1) }
     | float  { ThisAnnTy TFloat (annTy0 $1) }
     | string { ThisAnnTy TText (annTy0 $1) }
     | binary { ThisAnnTy TBytes (annTy0 $1) }
     | bool   { ThisAnnTy TBool (annTy0 $1) }
     | Symbol { ThisAnnTy (TNamed (lVal $1)) (Arity0Loc (lLoc $1)) }
     | map '<' AnnotatedType ',' AnnotatedType '>'
       { case ($3, $5) of
           (This k, This v) -> ThisAnnTy (TMap k v) (annTy2 $1 $2 $4 $6)
       }
     | hash_map '<' AnnotatedType ',' AnnotatedType '>'
       { case ($3, $5) of
           (This k, This v) -> ThisAnnTy (THashMap k v) (annTy2 $1 $2 $4 $6)
       }
     | set '<' AnnotatedType '>'
       { case $3 of
           This t -> ThisAnnTy (TSet t) (annTy1 $1 $2 $4)
       }
     | hash_set '<' AnnotatedType '>'
       { case $3 of
           This t -> ThisAnnTy (THashSet t) (annTy1 $1 $2 $4)
       }
     | list '<' AnnotatedType '>'
       { case $3 of
           This t -> ThisAnnTy (TList t) (annTy1 $1 $2 $4) }
     | stream '<' AnnotatedType '>'
       { case $3 of
           This t -> ThisAnnTy (TList t) (annTy1 $1 $2 $4) }

{

-- Result Types ----------------------------------------------------------------

data ThriftFile a l = ThriftFile
  { thriftName    :: Text
  , thriftPath    :: FilePath
  , thriftHeaders :: [Header l]
  , thriftDecls   :: [Decl 'Unresolved () l]
  , thriftSplice  :: a
  , thriftComments :: [Comment l]
  }

-- Parser Monad ----------------------------------------------------------------

type Parser = Alex

data L a = L { lLoc :: Located Loc, lVal :: a }

instance Functor L where
  fmap f (L loc val) = L loc $ f val

lParsed :: L (a, b) -> a
lParsed = fst . lVal

lRep :: L (a, b) -> b
lRep = snd . lVal

getLoc :: Token -> Located Loc
getLoc (Tok _ loc) = loc

runParser :: Parser a -> FilePath -> String -> Either String a
runParser parser file input = fst <$> runFullParser parser file input

runFullParser
  :: Parser a -> FilePath -> String -> Either String (a, [Comment Loc])
runFullParser parser file input =
  runAlex input $ setFilename file >> (,) <$> parser <*> getComments

parseError :: Token -> Parser a
parseError (Tok _ Located{lLocation=Loc{..}}) =
  alexError $ concat
    [locFile, ":", show locStartLine, ":", show locStartCol, ": parse error"]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind = (>>=)

lexWrap :: (Token -> Parser a) -> Parser a
lexWrap k = alexMonadScan >>= k

parseString :: FilePath -> String -> Either String (ThriftFile () Loc)
parseString file = fmap mkThriftFile . runFullParser parseThrift file
  where
    mkThriftFile ((headers, decls), comments) =
      ThriftFile
      { thriftName    = getModuleName file
      , thriftPath    = file
      , thriftHeaders = headers
      , thriftDecls   = decls
      , thriftSplice  = ()
      , thriftComments = comments
      }

parse :: FilePath -> FilePath -> IO (Either String (ThriftFile () Loc))
parse baseDir file = parseString file <$> readFile (baseDir </> file)

getModuleName :: FilePath -> Text
getModuleName file =
  fst . Text.breakOn "." .
  snd . Text.breakOnEnd "/" .
  Text.pack $ file

annTy0 :: Token -> TypeLoc 0 Loc
annTy0 tok = Arity0Loc $ getLoc tok

annTy1 :: Token -> Token -> Token -> TypeLoc 1 Loc
annTy1 tok open close = Arity1Loc
  { a1Ty         = getLoc tok
  , a1OpenBrace  = getLoc open
  , a1CloseBrace = getLoc close
  }

annTy2 :: Token -> Token -> Token -> Token -> TypeLoc 2 Loc
annTy2 tok open comma close = Arity2Loc
  { a2Ty         = getLoc tok
  , a2OpenBrace  = getLoc open
  , a2Comma      = getLoc comma
  , a2CloseBrace = getLoc close
  }
}
