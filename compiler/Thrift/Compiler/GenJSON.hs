-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
module Thrift.Compiler.GenJSON
  ( genJSON
  , writeJSON
  , getAstPath
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

writeJSON
  :: Typecheckable l
  => Program l a         -- ^ Top level program ti generate
  -> Maybe [Program l a] -- ^ Dependencies if using recursive mode
  -> IO FilePath
writeJSON prog@Program{..} deps = do
  createDirectoryIfMissing True dir
  LBS.writeFile path $ prettyJSON $ genJSON prog deps
  return path
  where
    path = dir </> file
    (dir, file) = getAstPath prog
    prettyJSON = encodePretty' defConfig { confCompare = compare }

getAstPath :: Program l a -> (FilePath, FilePath)
getAstPath Program{..} = (progOutPath, Text.unpack progName ++ ".ast")

genJSON :: Typecheckable l => Program l a -> Maybe [Program l a] -> Value
genJSON prog Nothing = Object $ genJSONProg prog
genJSON prog (Just deps) = toJSON $ genJSONProg prog : map genJSONProg deps

genJSONProg :: Typecheckable l => Program l a -> Object
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

-- Typedefs --------------------------------------------------------------------

genTypedef :: Typecheckable l => Typedef 'Resolved l a -> Object
genTypedef Typedef{..} = HashMap.fromList
  [ "name" .= tdResolvedName
  , "type" .= genType tdResolvedType
  , "newtype" .= case tdTag of { IsNewtype -> True ; IsTypedef -> False }
  ]

-- Enums -----------------------------------------------------------------------

genEnum :: Typecheckable l => Enum 'Resolved l a -> Object
genEnum Enum{..} = HashMap.fromList
  [ "name"      .= enumResolvedName
  , "constants" .= map genEnumConst enumConstants
  , "is_psuedo" .= enumIsPseudo
  ]

genEnumConst :: EnumValue 'Resolved l a -> Object
genEnumConst EnumValue{..} = HashMap.fromList
  [ "name"  .= evResolvedName
  , "value" .= evValue
  ]

-- Constants -------------------------------------------------------------------

genConst :: Typecheckable l => Const 'Resolved l a -> Object
genConst Const{..} = HashMap.fromList
  [ "name"  .= constResolvedName
  , "type"  .= genType constResolvedType
  , "value" .= genConstVal constResolvedType constResolvedVal
  ]

-- Structs, Exceptions, and Unions ---------------------------------------------

genStruct :: Typecheckable l => Struct 'Resolved l a -> Object
genStruct Struct{..} = HashMap.fromList
  [ "name" .= structResolvedName
  , "struct_type" .= case structType of
      StructTy    -> "STRUCT" :: Text
      ExceptionTy -> "EXCEPTION"
  , "fields" .= map genField structMembers
  ]

genField :: Typecheckable l => Field u 'Resolved l a -> Object
genField Field{..} = HashMap.fromList $
  [ "name"  .= fieldResolvedName
  , "id"    .= fieldId
  , "type"  .= genType fieldResolvedType
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

genUnion :: Typecheckable l => Union 'Resolved l a -> Object
genUnion Union{..} = HashMap.fromList
  [ "name"   .= unionResolvedName
  , "fields" .= map genAlt unionAlts
  ]

genAlt :: Typecheckable l => UnionAlt 'Resolved l a -> Object
genAlt UnionAlt{..} = HashMap.fromList
  [ "name" .= altResolvedName
  , "id"   .= altId
  , "type" .= genType altResolvedType
  ]

-- Services and Functions ------------------------------------------------------

genService :: Typecheckable l => Service 'Resolved l a -> Object
genService Service{..} = HashMap.fromList $
  [ "name"      .= serviceResolvedName
  , "functions" .= map genFunction serviceFunctions
  ] ++
  (case serviceSuper of
     Nothing -> []
     Just Super{..} -> [ "super" .= genName (fst supResolvedName) ])

genFunction :: Typecheckable l => Function 'Resolved l a -> Object
genFunction Function{..} = HashMap.fromList
  [ "name" .= funResolvedName
  , "return_type" .= case funResolvedType of
      Nothing -> simpleType "void"
      Just ty -> withSome ty genType
  , "args"   .= map genField funArgs
  , "throws" .= map genField funExceptions
  , "oneway" .= funIsOneWay
  ]

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
genType (TStruct name _loc)    = namedType "struct" name
genType (TException name _loc) = namedType "exception" name
genType (TUnion name _loc)     = namedType "union" name
genType (TEnum name _loc _) = namedType "enum" name
genType (TTypedef name ty _loc) = HashMap.fromList
  [ "type" .= ("typedef" :: Text)
  , "name" .= genName name
  , "inner_type" .= genType ty
  ]
genType (TNewtype name ty _loc) = HashMap.fromList
  [ "type" .= ("newtype" :: Text)
  , "name" .= genName name
  , "inner_type" .= genType ty
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

namedType :: Text -> Name -> Object
namedType tyName name = HashMap.fromList
  [ "type" .= tyName
  , "name" .= genName name
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
