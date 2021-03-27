-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP, NamedFieldPuns #-}
module Thrift.Compiler.GenJSONLoc
  ( -- * Main generation
    genJSONLoc
  , writeJSONLoc
  , getAstPathLoc
   -- * Utility functions
  , displayAnnotatedType
  , genType
  ) where

#if __GLASGOW_HASKELL__ > 804
#define This Some
#endif

import Prelude hiding (Enum)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy
import Data.Some
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy
import GHC.TypeLits
import System.Directory
import System.FilePath

import Thrift.Compiler.Typechecker
import Thrift.Compiler.Types as Types
import Thrift.Compiler.Options as Options
import Thrift.Compiler.Plugin
-- import Thrift.Compiler.Types as Thrift hiding (noLoc)

writeJSONLoc
  :: Typecheckable l
  => Program l Loc         -- ^ Top level program ti generate
  -> Maybe [Program l Loc] -- ^ Dependencies if using recursive mode
  -> IO FilePath
writeJSONLoc prog@Program{..} deps = do
  createDirectoryIfMissing True dir
  LBS.writeFile path $ prettyJSON $ genJSONLoc prog deps
  return path
  where
    path = dir </> file
    (dir, file) = getAstPathLoc prog
    prettyJSON = encodePretty' defConfig { confCompare = compare }

getAstPathLoc :: Program l a -> (FilePath, FilePath)
getAstPathLoc Program{..} = (progOutPath, Text.unpack progName ++ ".ast")

genJSONLoc :: Typecheckable l => Program l Loc -> Maybe [Program l Loc] -> Value
genJSONLoc prog Nothing = Object $ genJSONProg prog
genJSONLoc prog (Just deps) = toJSON $ genJSONProg prog : map genJSONProg deps

genJSONProg :: Typecheckable l => Program l Loc -> Object
genJSONProg Program{..} = HashMap.fromList
  [ "name"     .= progHSName
  , "path"     .= progPath
  , "includes" .= map Types.progPath progIncludes
  , "typedefs" .= map genTypedef dTdefs
  , "enums"    .= map genEnum dEnums
  , "consts"   .= map genConst dConsts
  , "structs"  .= map genStruct dStructs
  , "unions"   .= map genUnion dUnions
  , "services" .= map genService dServs
  , "options"  .= genOptions (options progEnv)
  ]
  where
    Decls{..} = partitionDecls progDecls

-- Locs ------------------------------------------------------------------------

tx :: Text -> Text
tx = id

displayLoc :: Loc -> Text
displayLoc = Text.pack . show

displayLocated :: Located Loc -> Text
displayLocated = displayLoc . lLocation

displayTypeLoc :: TypeLoc n Loc -> [Text]
displayTypeLoc x = map displayLocated $ case x of
   Arity0Loc{..} -> [a0Ty]
   Arity1Loc{..} -> [a1Ty, a1OpenBrace, a1CloseBrace]
   Arity2Loc{..} -> [a2Ty, a2OpenBrace, a2Comma, a2CloseBrace]

displaySeparator :: Separator Loc -> Object
displaySeparator (Semicolon loc) = HashMap.fromList
  [ "sep_type" .= tx "Semicolon"
  , "loc" .= displayLocated loc
  ]
displaySeparator (Comma loc) = HashMap.fromList
  [ "sep_type" .= tx "Comma"
  , "loc" .= displayLocated loc
  ]
displaySeparator NoSep = HashMap.fromList
  [ "sep_type" .= tx"NoSep"
  ]

displayAnnValue :: AnnValue -> Object
displayAnnValue (IntAnn i v) = HashMap.fromList
  [ "ann_value_type" .= tx "IntAnn"
  , "i" .= i
  , "v" .= v
  ]
displayAnnValue (TextAnn t q) = HashMap.fromList
  [ "ann_value_type" .=  tx "TextAnn"
  , "t" .= t
  , "q" .= Text.pack (show q)
  ]

displayAnnotation :: Annotation Loc -> Object
displayAnnotation SimpleAnn{..} = HashMap.fromList
  [ "ann_type" .= tx "SimpleAnn"
  , "ann_tag" .= saTag
  , "loc" .= displayLocated saLoc
  , "sep" .= displaySeparator saSep
  ]
displayAnnotation ValueAnn{..} = HashMap.fromList
  [ "ann_type" .= tx "ValueAnn"
  , "tag" .= vaTag
  , "val" .= displayAnnValue vaVal
  , "loc_tag" .= displayLocated vaTagLoc
  , "loc_equal" .= displayLocated vaEqual
  , "loc_val" .= displayLocated vaValLoc
  , "sep" .= displaySeparator vaSep
  ]

displayAnnotations :: Annotations Loc -> Object
displayAnnotations Annotations{..} = HashMap.fromList
  [ "loc_open" .= displayLocated annOpenParen
  , "loc_close" .= displayLocated annCloseParen
  , "loc_ann_list" .= map displayAnnotation annList
  ]

displayAnnotatedType :: forall t. AnnotatedType Loc t -> Object
displayAnnotatedType AnnotatedType{..} = HashMap.fromList
  [ "type" .= (genTType (atType :: Un t) :: Object)
  , "anns" .= maybe Null (Object . displayAnnotations) atAnnotations
  , "loc" .= displayTypeLoc atLoc
  ]

-- Reconstruct -----------------------------------------------------------------

-- The resolved Loc is in mkTypemap, mkSchemaMap, etc in typecheckModule.
-- I expect that the cross-ref are discovered in envLookup and envCtxLookup
-- when run during resolveDecls, but only implicitly.
--
-- Here the correspondance is reconstructed

reconstructXRef
  :: Typecheckable l
  =>  AnnotatedType Loc v
  -> Type l t
  -> [(Loc, Loc)]
reconstructXRef atIn@AnnotatedType{atType, atLoc} rt = case (atType, rt) of
    (TSet at1, TSet rt1) -> reconstructXRef at1 rt1
    (THashSet at1, THashSet rt1) -> reconstructXRef at1 rt1
    (TList at1, TList rt1) -> reconstructXRef at1 rt1
    (TMap ak av, TMap rk rv) ->
      reconstructXRef ak rk ++ reconstructXRef av rv
    (THashMap ak av, THashMap rk rv) ->
      reconstructXRef ak rk ++ reconstructXRef av rv
    (TNamed{}, TTypedef _ _ rtLoc) -> [(getTypeLoc atLoc, rtLoc)]
    (TNamed{}, TNewtype _ _ rtLoc) -> [(getTypeLoc atLoc, rtLoc)]
    (TNamed{}, TStruct _ rtLoc) -> [(getTypeLoc atLoc, rtLoc)]
    (TNamed{}, TException _ rtLoc) -> [(getTypeLoc atLoc, rtLoc)]
    (TNamed{}, TUnion _ rtLoc) -> [(getTypeLoc atLoc, rtLoc)]
    (TNamed{}, TEnum _ rtLoc _) -> [(getTypeLoc atLoc, rtLoc)]
    (_, TSpecial st) -> case backTranslateType st of
      (This rtSimple, _) -> reconstructXRef atIn rtSimple
    _ -> []

displayXRef
  ::  Typecheckable l
  =>  AnnotatedType Loc v
  -> Type l t
  -> [Value]
displayXRef at rt = map oneXRef (reconstructXRef at rt)

-- | JSON encode a pair of location, fst is usage and snd is definition. Want
-- to hyperlink from usage to destination, and perhaps list all usages of the
-- destination.
oneXRef :: (Loc, Loc) -> Value
oneXRef (aLoc, rLoc) = Object $ HashMap.fromList
  [ "aLoc" .= displayLoc aLoc
  , "rLoc" .= displayLoc rLoc ]

-- | Make this notice when the const value is another const or enum value
-- and reference the defintion. Enrich the Identifier and EnumVal
-- constructors to easily link them.
reconstructXRefConst
  :: Typecheckable l
  => UntypedConst Loc
  -> TypedConst l t
  -> Type l t
  -> [(Loc, Loc)]
reconstructXRefConst UntypedConst{ucLoc} (Identifier _name _rt rLoc) _ =
  [(lLocation ucLoc, rLoc)]
reconstructXRefConst UntypedConst{ucLoc} (WeirdEnumToInt _ _ _ rLoc) _ =
  [(lLocation ucLoc, rLoc)]
reconstructXRefConst UntypedConst{ucLoc} (Literal ev) (TEnum _ _loc _) =
  let EnumVal _name rLocVal = ev in [(lLocation ucLoc, rLocVal)]
reconstructXRefConst _ Literal{} _ = []

-- | Eventually make this notice when the const value is another const
-- and hyperlink that value (Identifier)
displayXRefConst
  :: Typecheckable l
  => UntypedConst Loc
  -> TypedConst l t
  -> Type l t
  -> [Value]
displayXRefConst uc tc ty = map oneXRef (reconstructXRefConst uc tc ty)

-- Typedefs --------------------------------------------------------------------

genTypedef :: Typecheckable l => Typedef 'Resolved l Loc -> Object
genTypedef Typedef{..} = HashMap.fromList
  [ "name" .= tdResolvedName
  , "type" .= genType tdResolvedType
  , "ann_type" .= displayAnnotatedType tdType
  , "newtype" .= case tdTag of { IsNewtype -> True ; IsTypedef -> False }
  , "loc_keyword" .= displayLocated (tdlKeyword tdLoc)
  , "loc_name" .= displayLocated (tdlName tdLoc)
  , "anns" .= maybe Null (Object . displayAnnotations) tdAnns
  , "xref" .= displayXRef tdType tdResolvedType
  ]

-- Enums -----------------------------------------------------------------------

genEnum :: Typecheckable l => Enum 'Resolved l Loc -> Object
genEnum Enum{..} = HashMap.fromList
  [ "name"      .= enumResolvedName
  , "constants" .= map genEnumConst enumConstants
  , "is_psuedo" .= enumIsPseudo
  , "loc_keyword" .= displayLocated (slKeyword enumLoc)
  , "loc_name" .= displayLocated (slName enumLoc)
  ]

genEnumConst :: EnumValue 'Resolved l Loc -> Object
genEnumConst EnumValue{..} = HashMap.fromList
  [ "name"  .= evResolvedName
  , "value" .= evValue
  , "loc_name" .= displayLocated (evlName evLoc)
  ]

-- Constants -------------------------------------------------------------------

genConst :: Typecheckable l => Const 'Resolved l Loc -> Object
genConst Const{..} = HashMap.fromList
  [ "name"  .= constResolvedName
  , "type"  .= genType constResolvedType
  , "value" .= genConstVal constResolvedType constResolvedVal
  , "ann_type" .= displayAnnotatedType constType
  , "loc_keyword" .= displayLocated (clKeyword constLoc)
  , "loc_name" .= displayLocated (clName constLoc)
  , "xref" .= (displayXRef constType constResolvedType
                ++ displayXRefConst constVal constResolvedVal constResolvedType)
  ]

-- Structs, Exceptions, and Unions ---------------------------------------------

genStruct :: Typecheckable l => Struct 'Resolved l Loc -> Object
genStruct Struct{..} = HashMap.fromList
  [ "name" .= structResolvedName
  , "struct_type" .= case structType of
      StructTy    -> "STRUCT" :: Text
      ExceptionTy -> "EXCEPTION"
  , "fields" .= map genField structMembers
  , "loc_keyword" .= displayLocated (slKeyword structLoc)
  , "loc_name" .= displayLocated (slName structLoc)
  ]

genField :: Typecheckable l => Field u 'Resolved l Loc -> Object
genField Field{..} = HashMap.fromList $
  [ "name"  .= fieldResolvedName
  , "id"    .= fieldId
  , "type"  .= genType fieldResolvedType
  , "xref"  .= displayXRef fieldType fieldResolvedType
  , "loc_name" .= displayLocated (flName fieldLoc)
  ] ++
  (case fieldResolvedVal of
     Nothing -> []
     Just val -> [ "default_value" .= genConstVal fieldResolvedType val ]) ++
  (case fieldTag of
     STRUCT_FIELD -> [ "requiredness" .=
                       case fieldRequiredness of
                         Default    -> "default" :: Text
                         Required{} -> "required"
                         Optional{} -> "optional"
                     ]
     _ -> [])

genUnion :: Typecheckable l => Union 'Resolved l Loc -> Object
genUnion Union{..} = HashMap.fromList
  [ "name"   .= unionResolvedName
  , "fields" .= map genAlt unionAlts
  , "loc_keyword" .= displayLocated (slKeyword unionLoc)
  , "loc_name" .= displayLocated (slName unionLoc)
  ]

genAlt :: Typecheckable l => UnionAlt 'Resolved l Loc -> Object
genAlt UnionAlt{..} = HashMap.fromList
  [ "name" .= altResolvedName
  , "id"   .= altId
  , "type" .= genType altResolvedType
  , "loc_name" .= displayLocated (flName altLoc)
  ]

-- Services and Functions ------------------------------------------------------

genService :: Typecheckable l => Service 'Resolved l Loc -> Object
genService Service{..} = HashMap.fromList $
  [ "name"      .= serviceResolvedName
  , "functions" .= map genFunction serviceFunctions
  , "loc_keyword" .= displayLocated (slKeyword serviceLoc)
  , "loc_name" .= displayLocated (slName serviceLoc)
  ] ++
  (case serviceSuper of
     Nothing -> []
     Just Super{..} -> [ "super" .= genName (fst supResolvedName) ])

genFunction :: Typecheckable l => Function 'Resolved l Loc -> Object
genFunction Function{..} = HashMap.fromList
  [ "name" .= funResolvedName
  , "return_type" .= case funResolvedType of
      Nothing -> simpleType "void"
      Just ty -> withSome ty genType
  , "args"   .= map genField funArgs
  , "throws" .= map genField funExceptions
  , "oneway" .= funIsOneWay
  , "loc_name" .= displayLocated (fnlName funLoc)
  ]


-- Unresolved Types and Constants ----------------------------------------------

type Un t = TType 'Unresolved () Loc t

genTType :: Un t -> Object

-- Base Types
genTType I8  = simpleType "byte"
genTType I16 = simpleType "i16"
genTType I32 = simpleType "i32"
genTType I64 = simpleType "i64"
genTType TFloat  = simpleType "float"
genTType TDouble = simpleType "double"
genTType TBool   = simpleType "bool"
genTType TText   = simpleType "string"
genTType TBytes  = simpleType "binary"

-- Collections
genTType (TSet u)       = collectionTType "set" u
genTType (THashSet u)   = collectionTType "hash_set" u
genTType (TList u)      = collectionTType "list" u
genTType (TMap k v)     = mapTType "map" k v
genTType (THashMap k v) = mapTType "hash_map" k v

-- Named Types

genTType (TNamed n) = simpleName n

collectionTType :: Text -> AnnotatedType Loc t -> Object
collectionTType tyName u = HashMap.fromList
  [ "type" .= tyName
  , "inner_type" .= displayAnnotatedType u
  ]

mapTType :: Text -> AnnotatedType Loc k -> AnnotatedType Loc v -> Object
mapTType tyName k v = HashMap.fromList
  [ "type" .= tyName
  , "key_type" .= displayAnnotatedType k
  , "val_type" .= displayAnnotatedType v
  ]

simpleName :: Text -> Object
simpleName tyName = HashMap.singleton "name" (String tyName)

-- Types and Constants ---------------------------------------------------------

genType :: Typecheckable l => Type l t -> Object

-- Base Types
genType I8  = simpleType "byte"
genType I16 = simpleType "i16"
genType I32 = simpleType "i32"
genType I64 = simpleType "i64"
genType TFloat  = simpleType "float"
genType TDouble = simpleType "double"
genType TBool   = simpleType "bool"
genType TText   = simpleType "string"
genType TBytes  = simpleType "binary"

-- Collections
genType (TSet u)       = collectionType "set" u
genType (THashSet u)   = collectionType "hash_set" u
genType (TList u)      = collectionType "list" u
genType (TMap k v)     = mapType "map" k v
genType (THashMap k v) = mapType "hash_map" k v

-- Named Types
genType (TStruct name loc)    = namedType "struct" name loc
genType (TException name loc) = namedType "exception" name loc
genType (TUnion name loc)     = namedType "union" name loc
genType (TEnum name loc _)      = namedType "enum" name loc
genType (TTypedef name ty loc) = HashMap.fromList
  [ "type" .= ("typedef" :: Text)
  , "name" .= genName name
  , "inner_type" .= genType ty
  , "loc" .= displayLoc loc
  ]
genType (TNewtype name ty loc) = HashMap.fromList
  [ "type" .= ("newtype" :: Text)
  , "name" .= genName name
  , "inner_type" .= genType ty
  , "loc" .= displayLoc loc
  ]
genType (TSpecial ty) = case backTranslateType ty of
  (This u, tag) -> genType u <> HashMap.fromList [ "special" .= tag ]

simpleType :: Text -> Object
simpleType tyName = HashMap.singleton "type" (String tyName)

collectionType :: Typecheckable l => Text -> Type l t -> Object
collectionType tyName u = HashMap.fromList
  [ "type" .= tyName
  , "inner_type" .= genType u
  ]

mapType :: Typecheckable l => Text -> Type l u -> Type l v -> Object
mapType tyName k v = HashMap.fromList
  [ "type" .= tyName
  , "key_type" .= genType k
  , "val_type" .= genType v
  ]

namedType :: Text -> Name -> Loc -> Object
namedType tyName name loc = HashMap.fromList
  [ "type" .= tyName
  , "name" .= genName name
  , "loc" .= displayLoc loc
  ]

genName :: Name -> Object
genName Name{..} = HashMap.fromList $
  [ "name" .= localName resolvedName ] ++
  [ "src" .= m | QName m _ <- [sourceName] ]

genConstVal :: Typecheckable l => Type l t -> TypedConst l t -> Object
genConstVal ty (Literal x) =
  HashMap.fromList [ "literal" .= genLiteral ty x ]
genConstVal _ (Identifier name _ _loc) =
  HashMap.fromList [ "named_constant" .= genName name ]
genConstVal _ (WeirdEnumToInt _ name _ _loc) =
  HashMap.fromList [ "named_constant_enumToInt" .= genName name ]

genLiteral :: Typecheckable l => Type l t -> t -> Object

-- Base Types
genLiteral ty@I8  n = simpleLiteral ty n
genLiteral ty@I16 n = simpleLiteral ty n
genLiteral ty@I32 n = simpleLiteral ty n
genLiteral ty@I64 n =
  -- We need to include the string representation because JSON does not support
  -- 64 bit integers
  simpleLiteral ty n <> HashMap.fromList [ "string" .= show n ]
genLiteral ty@TFloat n =
  simpleLiteral ty n <>
  HashMap.fromList [ "binary" .= toLazyText (floatHexFixed n) ]
genLiteral ty@TDouble n =
  simpleLiteral ty n <>
  HashMap.fromList [ "binary" .= toLazyText (doubleHexFixed n) ]
genLiteral ty@TBool b = simpleLiteral ty b
genLiteral ty@TText s = simpleLiteral ty s
-- Serialized as a hexidecimal string
genLiteral ty@TBytes s = simpleLiteral ty $ toLazyText $ byteStringHex s

-- Collections
genLiteral (TSet u)       (Set xs)     = listLiteral "set" u xs
genLiteral (THashSet u)   (HashSet xs) = listLiteral "hash_set" u xs
genLiteral (TList u)      (List xs)    = listLiteral "list" u xs
genLiteral (TMap k v)     (Map xs)     = mapLiteral "map" k v xs
genLiteral (THashMap k v) (HashMap xs) = mapLiteral "hash_map" k v xs

-- Named Types
genLiteral TStruct{} (This sval) = genStructVal sval
genLiteral TException{} (This (EV sval)) = genStructVal sval
genLiteral TUnion{} (This uval) = genUnionVal uval
genLiteral TEnum{} (EnumVal name _loc) = HashMap.fromList
  [ "type"  .= ("enum" :: Text)
  , "value" .= genName name
  ]
genLiteral (TTypedef _ ty _loc) x = genLiteral ty x
genLiteral (TNewtype _ ty _loc) (New x) = HashMap.fromList
  [ "type"  .= ("newtype" :: Text)
  , "value" .= genLiteral ty x
  ]
genLiteral st@(TSpecial ty) val = case backTranslateLiteral ty val of
  ThisLit u x -> HashMap.fromList
    [ "type"  .= genType st
    , "value" .= genLiteral u x
    ]

simpleLiteral :: (Typecheckable l, ToJSON a) => Type l t -> a -> Object
simpleLiteral ty x = genType ty <> HashMap.fromList [ "value" .= x ]

listLiteral :: Typecheckable l => Text -> Type l t -> [TypedConst l t] -> Object
listLiteral tyName ty xs = HashMap.fromList
  [ "type"  .= tyName
  , "value" .= map (genConstVal ty) xs
  ]

mapLiteral
  :: Typecheckable l
  => Text
  -> Type l k
  -> Type l v
  -> [(TypedConst l k, TypedConst l v)]
  -> Object
mapLiteral tyName kt vt xs = HashMap.fromList
  [ "type"  .= tyName
  , "value" .= map (genPair kt vt) xs
  ]

genPair
  :: Typecheckable l
  => Type l k
  -> Type l v
  -> (TypedConst l k, TypedConst l v)
  -> Value
genPair kt vt (k, v) = Object $ HashMap.fromList
  [ "key" .= genConstVal kt k
  , "val" .= genConstVal vt v
  ]

genStructVal :: Typecheckable l => StructVal l s -> Object
genStructVal s = HashMap.fromList
  [ "type"  .= ("struct" :: Text)
  , "value" .= genStructFields s
  ]

genStructFields :: Typecheckable l => StructVal l s -> [Object]
genStructFields Empty = []
genStructFields (ConsVal proxy ty c s) =
  genFieldVal proxy ty c : genStructFields s
genStructFields (ConsDefault proxy ty s) = HashMap.fromList
  [ "field_name"  .= symbolVal proxy
  , "field_type"  .= genType ty
  , "field_value" .=
    HashMap.singleton ("default" :: Text) Null
  ] :
  genStructFields s
genStructFields (ConsJust proxy ty c s) =
  genFieldVal proxy ty c : genStructFields s
genStructFields (ConsNothing proxy s) = HashMap.fromList
  [ "field_name"  .= symbolVal proxy
  , "field_value" .= Null
  ] :
  genStructFields s

genUnionVal :: Typecheckable l => UnionVal l s -> Object
genUnionVal (UnionVal proxy ty c _) = HashMap.fromList
  -- This isn't technically a thrift type, but we'll use it anyway
  [ "type"  .= ("union" :: Text)
  , "value" .= genFieldVal proxy ty c
  ]

genFieldVal
  :: (Typecheckable l, KnownSymbol s)
  => Proxy s
  -> Type l t
  -> TypedConst l t
  -> Object
genFieldVal proxy ty c = HashMap.fromList
  [ "field_name"  .= symbolVal proxy
  , "field_type"  .= genType ty
  , "field_value" .= genConstVal ty c
  ]

toLazyText :: Builder -> Lazy.Text
toLazyText = Lazy.decodeUtf8 . toLazyByteString

-- Options ---------------------------------------------------------------------

genOptions :: Options.Options l -> Object
genOptions Options.Options{..} = HashMap.fromList
  [ "path" .= optsPath
  , "out_path" .= optsOutPath
  , "include_path" .= optsIncludePath
  , "recursive" .= optsRecursive
  , "genfiles" .= optsThriftMade
  ]
