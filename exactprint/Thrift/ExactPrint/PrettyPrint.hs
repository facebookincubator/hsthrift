module Thrift.ExactPrint.PrettyPrint
  ( exactPrint
  , exactPrintThrift
  , exactPrintRaw
  , roundTrip
  ) where

import Data.Some
import qualified Data.Text as Strict
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Builder

import Thrift.Compiler.Parser
import Thrift.Compiler.Types

import Thrift.ExactPrint.Convert
import Thrift.ExactPrint.Types

exactPrint :: Program l Offset -> Text
exactPrint Program{..} = toLazyText $ mconcat
  (map ppHeader progHeaders ++
   map ppDecl progDecls ++
   map ppComment progComments) <> "\n"

exactPrintThrift :: ThriftFile a Offset -> Text
exactPrintThrift ThriftFile{..} = toLazyText $ mconcat
  (map ppHeader thriftHeaders ++
   map ppDecl thriftDecls ++
   map ppComment thriftComments) <> "\n"

exactPrintRaw :: [Decl s l Offset] -> Text
exactPrintRaw = toLazyText . mconcat . map ppDecl

roundTrip :: FilePath -> IO ()
roundTrip path = do
  result <- parse "." path
  case result of
    Left err -> putStrLn err
    Right ThriftFile{..} ->
      putStrLn $ Text.unpack $ exactPrintRaw $ declOffsets thriftDecls

-- Headers ---------------------------------------------------------------------

ppHeader :: Header Offset -> Builder
ppHeader HInclude{..} = mconcat
  [ addHeader incKeywordLoc
  , case incType of
      Include -> "include"
      HsInclude -> "hs_include"
      CppInclude -> "cpp_include"
  , addHeader incPathLoc
  , ppStr (Strict.pack incPath) incQuoteType
  ]
ppHeader HNamespace{..} = mconcat
  [ addHeader nmKeywordLoc, "namespace"
  , addHeader nmLangLoc, fromText nmLang
  , addHeader nmNameLoc
  , maybe fromText (flip ppStr) nmQuoteType nmName
  ]

-- Decls -----------------------------------------------------------------------

ppDecl :: Decl s l Offset -> Builder
ppDecl (D_Struct Struct{..}) = ppStruct structLoc
  (case structType of { StructTy -> "struct" ; ExceptionTy -> "exception" })
  structName
  (map ppField structMembers)
  structAnns

ppDecl (D_Union Union{..}) = ppStruct unionLoc "union" unionName
  (map ppUnionAlt unionAlts)
  unionAnns

ppDecl (D_Enum Enum{..}) = ppStruct enumLoc "enum" enumName
  (map ppEnumVal enumConstants)
  enumAnns

ppDecl (D_Typedef Typedef{tdLoc=TypedefLoc{..},..}) = mconcat
  [ addHeader tdlKeyword, "typedef"
  , ppType tdType
  , addHeader tdlName, fromText tdName
  , ppAnns tdAnns
  ]

ppDecl (D_Const Const{constLoc=ConstLoc{..},..}) = mconcat
  [ addHeader clKeyword, "const"
  , ppType constType
  , addHeader clName, fromText constName
  , addHeader clEqual, "="
  , ppConst constVal
  , ppSeparator clSeparator
  ]

ppDecl (D_Service Service{serviceLoc=StructLoc{..},..}) = mconcat $
  [ addHeader slKeyword, "service"
  , addHeader slName, fromText serviceName
  ] ++
  (case serviceSuper of
    Nothing -> []
    Just Super{..} ->
      [ addHeader supExtends, "extends"
      , addHeader supLoc, fromText supName
      ]) ++
  [ addHeader slOpenBrace, "{" ] ++
  map ppFunction serviceFunctions ++
  [ addHeader slCloseBrace, "}"
  , ppAnns serviceAnns
  ]

ppStruct
  :: StructLoc Offset
  -> Builder
  -> Strict.Text
  -> [Builder]
  -> Maybe (Annotations Offset)
  -> Builder
ppStruct StructLoc{..} keyword name fields anns = mconcat $
  [ addHeader slKeyword, keyword
  , addHeader slName, fromText name
  , addHeader slOpenBrace, "{"
  ] ++
  fields ++
  [ addHeader slCloseBrace, "}"
  , ppAnns anns
  ]

-- Fields ----------------------------------------------------------------------

ppField :: Field u s l Offset -> Builder
ppField Field{..} = mconcat
  [ addHeader flId, fromText flIdRep
  , addHeader flColon, ":"
  , case fieldRequiredness of
      Default -> ""
      Optional loc -> addHeader loc <> "optional"
      Required loc -> addHeader loc <> "required"
  , ppType fieldType
  , addHeader flName, fromText fieldName
  , maybe mempty (\loc -> addHeader loc <> "=") flEqual
  , maybe mempty ppConst fieldVal
  , ppAnns fieldAnns
  , ppSeparator flSeparator
  ]
  where
    FieldLoc{..} = fieldLoc

ppSeparator :: Separator Offset -> Builder
ppSeparator NoSep = ""
ppSeparator (Comma loc) = addHeader loc <> ","
ppSeparator (Semicolon loc) = addHeader loc <> ";"

-- Union Alts ------------------------------------------------------------------

ppUnionAlt :: UnionAlt s l Offset -> Builder
ppUnionAlt UnionAlt{altLoc=FieldLoc{..},..} = mconcat
  [ addHeader flId, fromText flIdRep
  , addHeader flColon, ":"
  , ppType altType
  , addHeader flName, fromText altName
  , ppAnns altAnns
  , ppSeparator flSeparator
  ]

-- Enum Values -----------------------------------------------------------------

ppEnumVal :: EnumValue s l Offset -> Builder
ppEnumVal EnumValue{evLoc=EnumValLoc{..},..} = mconcat
  [ addHeader evlName, fromText evName
  , addHeader evlEqual, "="
  , addHeader evlValue, fromText evlRep
  , ppAnns evAnns
  , ppSeparator evlSeparator
  ]

-- Constants -------------------------------------------------------------------

ppConst :: UntypedConst Offset -> Builder
ppConst UntypedConst{..} =
  addHeader ucLoc <> case ucConst of
    IntVal _ rep -> fromText rep
    DoubleVal _ rep -> fromText rep
    BoolVal True -> "true"
    BoolVal False -> "false"
    StringVal s qt -> ppStr s qt
    IdVal i -> fromText i
    ListVal{..} -> mconcat $
      "[" :
      map (ppListElem ppConst) lvElems ++
      [ addHeader lvCloseBrace, "]" ]
    MapVal{..} -> mconcat $
      "{" :
      map (ppListElem ppMapPair) mvElems ++
      [ addHeader mvCloseBrace, "}" ]

ppListElem :: (f Offset -> Builder) -> ListElem f Offset -> Builder
ppListElem pp ListElem{..} = pp leElem <> ppSeparator leSeparator

ppMapPair :: MapPair Offset -> Builder
ppMapPair MapPair{..} = mconcat
  [ ppConst mpKey
  , addHeader mpColon, ":"
  , ppConst mpVal
  ]

-- Functions -------------------------------------------------------------------

ppFunction :: Function s l Offset -> Builder
ppFunction Function{funLoc=FunLoc{..},..} = mconcat $
  [ case fnlOneway of { Nothing -> "" ; Just loc -> addHeader loc <> "oneway" }
  , case funType of
     Left loc -> addHeader loc <> "void"
     Right (This ty) -> ppType ty
  , addHeader fnlName, fromText funName
  , addHeader fnlOpenParen, "("
  ] ++
  map ppField funArgs ++
  [ addHeader fnlCloseParen, ")"
  , maybe mempty ppThrows fnlThrows
  , ppAnns funAnns
  , ppSeparator fnlSeparator
  ]
  where
    ppThrows ThrowsLoc{..} = mconcat $
      [ addHeader tlThrows, "throws"
      , addHeader tlOpenParen, "("
      ] ++
      map ppField funExceptions ++
      [ addHeader tlCloseParen, ")"
      ]

-- Types -----------------------------------------------------------------------

ppType :: AnnotatedType Offset t -> Builder
ppType AnnotatedType{..} =
  (case atType of
    -- Arity 0 Types
    I8 -> ppType0 atLoc "byte"
    I16 -> ppType0 atLoc "i16"
    I32 -> ppType0 atLoc "i32"
    I64 -> ppType0 atLoc "i64"
    TFloat -> ppType0 atLoc "float"
    TDouble -> ppType0 atLoc "double"
    TBool -> ppType0 atLoc "bool"
    TText -> ppType0 atLoc "string"
    TBytes -> ppType0 atLoc "binary"
    TNamed n -> ppType0 atLoc $ fromText n

    -- Arity 1 Types
    TList u -> ppType1 atLoc "list" $ ppType u
    TSet u -> ppType1 atLoc "set" $ ppType u
    THashSet u -> ppType1 atLoc "hash_set" $ ppType u

    -- Arity 2 Types
    TMap k v -> ppType2 atLoc "map" (ppType k) (ppType v)
    THashMap k v -> ppType2 atLoc "hash_map" (ppType k) (ppType v)) <>
  ppAnns atAnnotations

ppType0 :: TypeLoc 0 Offset -> Builder -> Builder
ppType0 Arity0Loc{..} ty = addHeader a0Ty <> ty

ppType1 :: TypeLoc 1 Offset -> Builder -> Builder -> Builder
ppType1 Arity1Loc{..} ty inner = mconcat
  [ addHeader a1Ty, ty
  , addHeader a1OpenBrace, "<"
  , inner
  , addHeader a1CloseBrace, ">"
  ]

ppType2 :: TypeLoc 2 Offset -> Builder -> Builder -> Builder -> Builder
ppType2 Arity2Loc{..} ty u v = mconcat
  [ addHeader a2Ty, ty
  , addHeader a2OpenBrace, "<"
  , u
  , addHeader a2Comma, ","
  , v
  , addHeader a2CloseBrace, ">"
  ]

ppAnns :: Maybe (Annotations Offset) -> Builder
ppAnns Nothing = mempty
ppAnns (Just Annotations{..}) = mconcat $
  [ addHeader annOpenParen, "(" ] ++
  map ppAnn annList ++
  [ addHeader annCloseParen, ")" ]

ppAnn :: Annotation Offset -> Builder
ppAnn SimpleAnn{..} = addHeader saLoc <> fromText saTag <> ppSeparator saSep
ppAnn ValueAnn{..} = mconcat
  [ addHeader vaTagLoc, fromText vaTag
  , addHeader vaEqual, "="
  , addHeader vaValLoc
  , case vaVal of
      TextAnn txt qt -> ppStr txt qt
      IntAnn _ rep -> fromText rep
  , ppSeparator vaSep
  ]

ppStr :: Strict.Text -> QuoteType -> Builder
ppStr txt qt = quote <> fromText txt <> quote
  where quote = case qt of { SingleQuote -> "'" ; DoubleQuote -> "\"" }

-- Helpers ---------------------------------------------------------------------

addHeader :: Located Offset -> Builder
addHeader Located{..} =
  mconcat (map ppComment lComments) <> ppOffset lLocation

ppComment :: Comment Offset -> Builder
ppComment (Comment offs txt) = ppOffset offs <> fromText txt

ppOffset :: Offset -> Builder
ppOffset Offset{..} = mconcat $
  replicate offsRows (singleton '\n') ++
  replicate offsCols (singleton ' ')
