-- Copyright (c) Facebook, Inc. and its affiliates.

module Thrift.Compiler.GenStruct
  ( genStructDecl
  , genStructImports
  -- * Helpers
  , fixToJSONValue
  , ParseMode(..)
  , genFieldParser
  , genBuildValue, genBuildFields, genFieldBase, genParseType
  , getUn
  , transformValue, mkHashable, mkOrd
  ) where

import Prelude hiding (exp)
import Data.Maybe
import Data.Set (union)
import Data.Text (Text)
import Language.Haskell.Exts.Syntax hiding
  (Name, Type, Annotation, Decl)
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Language.Haskell.Exts.Syntax as HS

import Thrift.Compiler.GenUtils
import Thrift.Compiler.Plugins.Haskell
import Thrift.Compiler.Types

-- Generate Datatype -----------------------------------------------------------

genStructImports :: HS Struct -> Set.Set Import
genStructImports Struct{..} =
  foldr (Set.union . getImports) baseImports structMembers
  where
    getImports :: HS (Field u) -> Set.Set Import
    getImports Field{..} = typeToImport fieldResolvedType
    baseImports = Set.fromList
                  [ QImport "Prelude" "Prelude"
                  , QImport "Control.DeepSeq" "DeepSeq"
                  , QImport "Control.Monad" "Monad"
                  , QImport "Control.Monad.ST.Trans" "ST"
                  , QImport "Control.Monad.Trans.Class" "Trans"
                  , QImport "Data.Aeson" "Aeson"
                  , QImport "Data.Aeson.Types" "Aeson"
                  , QImport "Data.Default" "Default"
                  , QImport "Data.HashMap.Strict" "HashMap"
                  , QImport "Data.Hashable" "Hashable"
                  , QImport "Data.List" "List"
                  , QImport "Data.Ord" "Ord"
                  , QImport "Thrift.Binary.Parser" "Parser"
                  , SymImport "Prelude"
                    [ ".", "<$>", "<*>", ">>=", "==", "/=", "<", "++" ]
                  , SymImport "Control.Applicative" [ "<|>", "*>", "<*" ]
                  , SymImport "Data.Aeson" [ ".:", ".:?", ".=", ".!=" ]
                  , SymImport "Data.Monoid" [ "<>" ]
                  ] `union`
                  (case structType of
                     StructTy -> Set.empty
                     ExceptionTy ->
                       Set.singleton $
                       QImport "Control.Exception" "Exception")

genStructDecl :: Bool -> HS Struct -> [HS.Decl ()]
genStructDecl extraHasFields struct@Struct{..} =
  -- Struct Declaration
  [ DataDecl () (dataOrNew ()) Nothing
    (DHead () $ textToName structResolvedName)
    -- Record Constructor
    [QualConDecl () Nothing Nothing
      (RecDecl () (textToName structResolvedName) (map genFieldDecl structMembers))
    ]
    -- Deriving
    (pure $ deriving_ $ map (IRule () Nothing Nothing . IHCon ()) $
           [ qualSym "Prelude" "Eq"
           , qualSym "Prelude" "Show"
           ] ++
           [ qualSym "Prelude" "Ord" | deriveOrd ])
  -- Aeson Instances
  , genToJSONInst struct
  -- ThriftStruct Instance
  , genThriftStruct struct
  -- Other Instances
  , genNFData struct
  , genDefault struct
  , genHashable struct
  ] ++
  [ genOrd struct | not deriveOrd ] ++
  (case structType of
    StructTy -> []
    ExceptionTy -> [ genException struct ]) ++
  (if extraHasFields
    then genExtraHasFields struct
    else []
  )
 where
   dataOrNew = case structMembers of
     -- Make the struct a newtype iff it has exactly one element
     [_] -> NewType
     _ -> DataType
   deriveOrd = canDeriveOrd struct

genFieldType :: HS (Field u) -> HS.Type ()
genFieldType Field{..} = opTy
  where
    baseTy = genType fieldResolvedType
    opTy =
      case fieldRequiredness of
        Optional{} -> TyApp () (qualType "Prelude" "Maybe") baseTy
        _          -> baseTy

genFieldDecl :: HS (Field u) -> FieldDecl ()
genFieldDecl field@Field{..} = FieldDecl () [textToName fieldResolvedName] ty
  where
    baseTy = genFieldType field
    ty =
      case fieldLaziness of
        Lazy -> baseTy
        Strict | isBaseType fieldResolvedType ->
                   TyBang () (BangedTy ()) (Unpack ()) baseTy
               | otherwise            ->
                   TyBang () (BangedTy ()) (NoUnpack ()) baseTy

-- Generate Aeson Instances ----------------------------------------------------

genToJSONInst :: HS Struct -> HS.Decl ()
genToJSONInst struct@Struct{..} =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym "Aeson" "ToJSON")
       (TyCon () $ unqualSym structResolvedName))
    (Just $ map (InsDecl ())
     [ genToJSON "toJSON" ":" (qvar "Aeson" "object") struct
     ])

genToJSON :: Text -> Text -> Exp () -> HS Struct -> HS.Decl ()
genToJSON name op combine Struct{..} =
  FunBind ()
  [ Match ()
    (textToName name)
    [ PApp () (unqualSym structResolvedName) $
      map (\Field{..} -> pvar ("__field__" <> fieldName)) structMembers
    ]
    (UnGuardedRhs () $
     combine `app`
     foldr (genToJSONField op) (qvar "Prelude" "mempty") structMembers)
    Nothing
  ]

genToJSONField :: Text -> HS (Field u) -> Exp () -> Exp ()
genToJSONField op Field{..} exps =
  case fieldRequiredness of
    Optional{} ->
      qvar "Prelude" "maybe" `app`
      qvar "Prelude" "id" `app`
      (Var () (UnQual () (Symbol () (Text.unpack op))) `compose`
       LeftSection () (stringLit fieldName)
       (QVarOp () (UnQual () (Symbol () ".=")))) `app`
      fieldValue `app`
      exps
    _ ->
      infixApp op
        (infixApp ".=" (stringLit fieldName) fieldValue)
        exps
  where
    fieldValue =
      case (fieldRequiredness, fixToJSONValue fieldResolvedType) of
        (_, Nothing) -> val
        (Optional{}, Just f) -> qvar "Prelude" "fmap" `app` f `app` val
        (_         , Just f) -> f `app` val

    val = var $ "__field__" <> fieldName

fixToJSONValue :: HSType t -> Maybe (Exp ())
fixToJSONValue I8 = Nothing
fixToJSONValue I16 = Nothing
fixToJSONValue I32 = Nothing
fixToJSONValue I64 = Nothing
fixToJSONValue (TSpecial HsInt) = Nothing
fixToJSONValue TFloat = Nothing
fixToJSONValue TDouble = Nothing
fixToJSONValue TBool = Nothing
fixToJSONValue (TSpecial HsString) = Nothing
fixToJSONValue (TSpecial HsByteString) = Just $ qvar "Text" "decodeUtf8"
fixToJSONValue TText = Nothing
fixToJSONValue TBytes = Just $ qvar "Thrift" "encodeBase64Text"
fixToJSONValue (TList u)    = app (qvar "Prelude" "map") <$> fixToJSONValue u
fixToJSONValue (TSpecial (HsVector vec u)) =
  app (qvar (hsVectorQual vec) "map") <$> fixToJSONValue u
fixToJSONValue (TSet u)     = app (qvar "Set" "map") <$> fixToJSONValue u
fixToJSONValue (THashSet u) = app (qvar "HashSet" "map") <$> fixToJSONValue u
fixToJSONValue (TMap k v) =
  fixToJSONMap (qvar "Map" "mapKeys") (qvar "Map" "map") k v
fixToJSONValue (THashMap k v) =
  fixToJSONMap (qvar "Thrift" "hmMapKeys") (qvar "HashMap" "map") k v
fixToJSONValue TEnum{} = Nothing
fixToJSONValue TStruct{} = Nothing
fixToJSONValue TException{} = Nothing
fixToJSONValue TUnion{} = Nothing
fixToJSONValue (TTypedef _ ty _loc) = fixToJSONValue ty
fixToJSONValue (TNewtype name ty _loc) = Just $
  maybe id compose (fixToJSONValue ty) $ getUn name

fixToJSONMap
  :: Exp () -> Exp () -> HSType k -> HSType v -> Maybe (Exp ())
fixToJSONMap mapK mapV k v
  | t:ts <- catMaybes [ keyToStr, fixK, fixV ] =
      Just $ foldl compose t ts
  | otherwise = Nothing
  where
    keyToStr
      | isAesonKey k = Nothing
      | otherwise = Just $ mapK `app` qvar "Thrift" "keyToStr"
    fixK = app mapK <$> fixToJSONValue k
    fixV = app mapV <$> fixToJSONValue v

getUn :: Name -> Exp ()
getUn Name{..} = case resolvedName of
  UName name -> var $ "un" <> name
  QName m name -> qvar m $ "un" <> name

isAesonKey :: HSType t -> Bool
isAesonKey TText = True
isAesonKey (TSpecial HsString) = True
-- ByteString isn't actually an Aeson key, but it will be transformed to Text by
-- fixToJSONValue, so we don't want to encode it twice
isAesonKey TBytes = True
isAesonKey (TSpecial HsByteString) = True
isAesonKey _ = False

-- Generate ThriftStruct Instance ----------------------------------------------

genThriftStruct :: HS Struct -> HS.Decl ()
genThriftStruct struct@Struct{..} =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym "Thrift" "ThriftStruct")
       (TyCon () $ unqualSym structResolvedName))
    (Just $ map (InsDecl ())
     [ genBuilder struct
     , genParser struct
     ])

data ParseMode = P_FieldMode | P_ListMode

insertParens :: ParseMode -> Exp () -> Exp ()
insertParens P_FieldMode = id
insertParens P_ListMode = Paren ()

genBuilder :: HS Struct -> HS.Decl ()
genBuilder Struct{..} =
  FunBind ()
  [ Match ()
    (textToName "buildStruct")
    [ PVar () $ textToName "_proxy"
    , PApp () (unqualSym structResolvedName) $
      map (PVar () . textToName . ("__field__" <>) . fieldName) structMembers
    ]
    (UnGuardedRhs () $ genBuildFields structMembers)
    Nothing
  ]

genBuildFields :: [HS (Field u)] -> Exp ()
genBuildFields fields =
  protocolFun "genStruct" `app`
  (case genBuildField NoField fields of
     NoField -> id
     ReqField _ f -> f
     OptField _ f -> f)
  (HS.List () [])

data PreviousField
   = NoField
   -- Both ReqField and OptField contain continuations for building the output
   -- of the function
   | ReqField FieldId (Exp () -> Exp ())
   | OptField Text (Exp () -> Exp ())

genBuildField :: PreviousField -> [HS (Field u)] -> PreviousField
genBuildField prev [] = prev
genBuildField prev (Field{..} : fs) = flip genBuildField fs $
  case fieldRequiredness of
    Optional{} | null fs ->
      ReqField fieldId $ \e -> combine $
        Case () (var ("__field__" <> fieldName))
        [ Alt () (PApp () (qualSym "Prelude" "Just") [ pvar "_val" ])
          (UnGuardedRhs () $
           (infixApp ":"
            (genField $ var "_val")
            e))
          Nothing
        , Alt () (PApp () (qualSym "Prelude" "Nothing") [])
          (UnGuardedRhs () $ e)
          Nothing
        ]
    Optional{} ->
      OptField fieldName $ \e -> combine $
        Let ()
          (BDecls ()
           [ PatBind () (PTuple () Boxed
                         [ pvar ("__cereal__" <> fieldName)
                         , pvar ("__id__" <> fieldName)
                         ])
             (UnGuardedRhs () $
              Case () (var ("__field__" <> fieldName))
              [ Alt () (PApp () (qualSym "Prelude" "Just") [ pvar "_val" ])
                (UnGuardedRhs () $
                 Tuple () Boxed
                 [ Con () (UnQual () (Symbol () ":")) `app`
                   genField (var "_val")
                 , intLit fieldId
                 ])
                Nothing
              , Alt () (PApp () (qualSym "Prelude" "Nothing") [])
                (UnGuardedRhs () $
                 Tuple () Boxed
                 [ qvar "Prelude" "id"
                 , lastId
                 ])
                Nothing
              ])
             Nothing
           ]) $
            (var $ "__cereal__" <> fieldName) `app` e
    _ -> ReqField fieldId $
         combine .
         infixApp ":"
         (genField $ var $ "__field__" <> fieldName)
  where
    genField = genFieldBase fieldResolvedType fieldName fieldId lastId
    (lastId, combine) =
      case prev of
        NoField -> (intLit (0 :: FieldId), id)
        ReqField fid f -> (intLit fid, f)
        OptField name f -> (var $ "__id__" <> name, f)

genFieldBase :: HSType t -> Text -> FieldId -> Exp () -> Exp () -> Exp ()
genFieldBase ty name fid lastId arg =
  case getPrim ty of
   Nothing ->
     protocolFun "genField" `app`
     stringLit name `app`
     genThriftType ty `app`
     intLit fid `app`
     lastId `app`
     (genBuildValue ty `app` arg)
   Just P_Bool ->
     protocolFun "genFieldBool" `app`
     stringLit name `app`
     intLit fid `app`
     lastId `app`
     arg
   Just primTy ->
     protocolFun "genFieldPrim" `app`
     stringLit name `app`
     genThriftType ty `app`
     intLit fid `app`
     lastId `app`
     genBuildPrim primTy `app`
     arg

genBuildValue :: HSType t -> Exp ()
-- Primatives
genBuildValue I8  = protocolFun "genByte"
genBuildValue I16 = protocolFun "genI16"
genBuildValue I32 = protocolFun "genI32"
genBuildValue I64 = protocolFun "genI64"
genBuildValue (TSpecial HsInt) =
  protocolFun "genI64" `compose`
  qvar "Prelude" "fromIntegral"
genBuildValue TFloat  = protocolFun "genFloat"
genBuildValue TDouble = protocolFun "genDouble"
genBuildValue TBool = protocolFun "genBool"
genBuildValue TText = protocolFun "genText"
genBuildValue (TSpecial HsString) =
  genBuildValue TText `compose` qvar "Text" "pack"
genBuildValue (TSpecial HsByteString) = protocolFun "genByteString"
genBuildValue TBytes = protocolFun "genBytes"
-- Containers
genBuildValue (TList ty) = genBuildList ty
genBuildValue (TSpecial (HsVector vec ty)) =
  genBuildList ty `compose` qvar (hsVectorQual vec) "toList"
genBuildValue (TSet ty) =
  genBuildList ty `compose` qvar "Set" "toList"
genBuildValue (THashSet ty) =
  genBuildList ty `compose` qvar "HashSet" "toList"
genBuildValue (TMap k v) =
  genBuildMap k v `compose` qvar "Map" "toList"
genBuildValue (THashMap k v) =
  genBuildMap k v `compose` qvar "HashMap" "toList"
-- Named Types
genBuildValue (TTypedef _ ty _loc) = genBuildValue ty
genBuildValue (TNewtype name ty _loc) =
  genBuildValue ty `compose` getUn name
genBuildValue (TStruct _ _loc) = protocolFun "buildStruct"
genBuildValue (TException _ _loc) = protocolFun "buildStruct"
genBuildValue (TUnion _ _loc) = protocolFun "buildStruct"
genBuildValue (TEnum _ _loc _) =
  protocolFun "genI32" `compose`
  qvar "Prelude" "fromIntegral" `compose`
  qvar "Thrift" "fromThriftEnum"

genBuildPrim :: PrimType -> Exp ()
genBuildPrim P_I8 = protocolFun "genBytePrim"
genBuildPrim P_I16 = protocolFun "genI16Prim"
genBuildPrim P_I32 = protocolFun "genI32Prim"
genBuildPrim P_I64 = protocolFun "genI64Prim"
genBuildPrim P_Bool = protocolFun "genBoolPrim"

genBuildList :: HSType t -> Exp ()
genBuildList ty =
  case getPrim ty of
    Just primTy ->
      protocolFun "genListPrim" `app`
      genThriftType ty `app`
      genBuildPrim primTy
    Nothing ->
      protocolFun "genList" `app`
      genThriftType ty `app`
      genBuildValue ty

genBuildMap :: HSType k -> HSType v -> Exp ()
genBuildMap k v =
  case (getPrim k, getPrim v) of
    (Just primK, Just primV) ->
      protocolFun "genMapPrim" `app`
      genThriftType k `app`
      genThriftType v `app`
      qcon "Prelude" (if isStringType k then "True" else "False") `app`
      genBuildPrim primK `app`
      genBuildPrim primV
    _ ->
      protocolFun "genMap" `app`
      genThriftType k `app`
      genThriftType v `app`
      qcon "Prelude" (if isStringType k then "True" else "False") `app`
      genBuildValue k `app`
      genBuildValue v

isStringType :: HSType t -> Bool
isStringType (TSpecial HsString) = True
isStringType (TSpecial HsByteString) = True
isStringType TText = True
isStringType TBytes = True
isStringType (TTypedef _ t _loc) = isStringType t
isStringType (TNewtype _ t _loc) = isStringType t
isStringType _ = False

genFieldParser
  :: [HS (Field u)] -> Text -> (Exp () -> Exp ()) -> Rhs ()
genFieldParser fields constructorName constructorWrapper = UnGuardedRhs () $
  qvar "ST" "runSTT" `app`
  Do ()
  -- We need this because of some issue in runST (t10447741)
  ( Qualifier () (qvar "Prelude" "return" `app` unit_con ()) :
  -- field <- newSTRef Nothing
  map
   (\field@Field{..} ->
    Generator () (pvar ("__field__" <> fieldName))
    (qvar "ST" "newSTRef" `app`
     case fieldRequiredness of
       Default -> genFieldDefault field
       _ -> qcon "Prelude" "Nothing"))
   fields ++
   -- let __parse = do {..}
   [ LetStmt () $
     BDecls ()
     [ FunBind ()
       -- the __parse function recursively builds the output and stores the
       -- parsed values in STRefs that are initialized in the outer scope
       [ Match () (textToName "_parse") [ pvar "_lastId" ]
         (UnGuardedRhs () $ Do ()
          -- Field Case: pattern match on parsed field id
          [ Generator () (pvar "_fieldBegin") $
            qvar "Trans" "lift" `app`
            (protocolFun "parseFieldBegin" `app`
             var "_lastId" `app`
             var "_idMap")
          , Qualifier () $ Case () (var "_fieldBegin")
            [ Alt ()
              (PApp () (qualSym "Thrift" "FieldBegin")
               [ pvar "_type", pvar "_id", pvar "_bool" ])
              (UnGuardedRhs () $ Do ()
               [ Qualifier () $ Case () (var "_id") $
                 map genParseValue fields ++
                 [ Alt () (PWildCard ())
                   (UnGuardedRhs () $
                    qvar "Trans" "lift" `app`
                    (protocolFun "parseSkip" `app`
                     var "_type" `app`
                     (qcon "Prelude" "Just" `app` var "_bool")))
                   Nothing
                 ]
               , Qualifier () $ var "_parse" `app` var "_id"
               ])
              Nothing
            , Alt () (PApp () (qualSym "Thrift" "FieldEnd") [])
              (UnGuardedRhs () $ Do () $
               -- Get the values from the STRefs
               map (\Field{..} ->
                    Generator ()
                    (PBangPat () $ case fieldRequiredness of
                       Required{} -> pvar $ "__maybe__" <> fieldName
                       _ -> pvar $ "__val__" <> fieldName) $
                    qvar "ST" "readSTRef" `app` var ("__field__" <> fieldName))
               fields ++
               [ Qualifier () $
                 foldr matchArg buildOutput fields
               ])
              Nothing
            ]
          ])
         Nothing
       ]
     , PatBind () (pvar "_idMap")
       (UnGuardedRhs () $
        qvar "HashMap" "fromList" `app`
        HS.List ()
        (map (\Field{..} ->
         Tuple () Boxed [ stringLit fieldName, intLit fieldId ])
        fields))
       Nothing
       ]
   -- call _parse
   , Qualifier () $ var "_parse" `app` intLit (0 :: Int)
   ])
  where
    buildOutput =
      qvar "Prelude" "pure" `app` constructorWrapper
      (foldl app (con constructorName) (map mkFieldName fields))
    mkFieldName Field{..} = var $ "__val__" <> fieldName
    matchArg :: HS (Field u) -> Exp () -> Exp ()
    matchArg Field{..} exp =
      case fieldRequiredness of
        Required{} ->
          Case () (var $ "__maybe__" <> fieldName)
          [ Alt () (PApp () (qualSym "Prelude" "Nothing") [])
            (UnGuardedRhs () $
             qvar "Prelude" "fail" `app`
             stringLit
               ("Error parsing type "
               <> constructorName
               <> ": missing required field " <> fieldName
               <> " of type " <> Text.pack (
                 prettyPrint $ genType fieldResolvedType)
               ))
            Nothing
          , Alt () (PApp () (qualSym "Prelude" "Just")
                    [ pvar ("__val__" <> fieldName) ])
            (UnGuardedRhs () exp)
            Nothing
          ]
        _ -> exp

genParser :: HS Struct -> HS.Decl ()
genParser Struct{..} =
  FunBind ()
  [ Match ()
    (textToName "parseStruct")
    [ pvar "_proxy" ]
    (genFieldParser structMembers structResolvedName (Paren ()))
    Nothing
  ]

genParseValue :: HS (Field u) -> Alt ()
genParseValue Field{..} =
  Alt ()
  (PLit ()
   (if fieldId < 0 then Negative () else Signless ())
   (Int () (abs $ fromIntegral fieldId) (show fieldId)))
  (GuardedRhss ()
   -- check that the parsed type is correct
   [ GuardedRhs ()
     [ Qualifier () $
       infixApp "==" (var "_type") (genThriftType fieldResolvedType)
     ] $
     Do ()
     [ Generator () (PBangPat () $ pvar "_val") $
       qvar "Trans" "lift" `app` genParseType P_FieldMode fieldResolvedType
     , Qualifier () $
       qvar "ST" "writeSTRef" `app`
       var ("__field__" <> fieldName) `app`
       case fieldRequiredness of
         Default -> var "_val"
         _ -> qcon "Prelude" "Just" `app` var "_val"
     ]
   ])
  Nothing

genParseType :: ParseMode -> HSType t -> Exp ()
-- Base types
genParseType _ I8  = protocolFun "parseByte"
genParseType _ I16 = protocolFun "parseI16"
genParseType _ I32 = protocolFun "parseI32"
genParseType _ I64 = protocolFun "parseI64"
genParseType m (TSpecial HsInt) = insertParens m $
  infixApp "<$>" (qvar "Prelude" "fromIntegral") (protocolFun "parseI64")
genParseType _ TFloat  = protocolFun "parseFloat"
genParseType _ TDouble = protocolFun "parseDouble"
genParseType P_FieldMode TBool = protocolFun "parseBoolF" `app` var "_bool"
genParseType P_ListMode  TBool = protocolFun "parseBool"
genParseType _ TText = protocolFun "parseText"
genParseType m (TSpecial HsString) = insertParens m $
  infixApp "<$>" (qvar "Text" "unpack") (protocolFun "parseText")
genParseType _ (TSpecial HsByteString) = protocolFun "parseByteString"
genParseType _ TBytes = protocolFun "parseBytes"
-- Containers
genParseType _ (TList ty) =
  infixApp "<$>" (qvar "Prelude" "snd") (genParseList ty)
genParseType m (TSpecial (HsVector vec ty)) = insertParens m $
  infixApp "<$>"
    (qvar "Prelude" "uncurry" `app` qvar (hsVectorQual vec) "fromListN")
    (genParseList ty)
genParseType m (TSet ty) = insertParens m $
  infixApp "<$>"
    (qvar "Set" "fromList" `compose` qvar "Prelude" "snd")
    (genParseList ty)
genParseType m (THashSet ty) = insertParens m $
  infixApp "<$>"
    (qvar "HashSet" "fromList" `compose` qvar "Prelude" "snd")
    (genParseList ty)
genParseType m (TMap k v) = insertParens m $
  infixApp "<$>" (qvar "Map" "fromList") (genParseMap k v)
genParseType m (THashMap k v) = insertParens m $
  infixApp "<$>" (qvar "HashMap" "fromList") (genParseMap k v)
-- Named Types
genParseType m (TTypedef _ ty _loc) = genParseType m ty
genParseType m (TNewtype name ty _loc) =
  qvar "Prelude" "fmap" `app` Con () (nameToQName name) `app` genParseType m ty
genParseType _ TStruct{} = protocolFun "parseStruct"
genParseType _ TException{} = protocolFun "parseStruct"
genParseType _ TUnion{} = protocolFun "parseStruct"
genParseType _ (TEnum Name{..} _loc nounknown) =
  qvar "Thrift" (parseEnumFun nounknown) `app`
  var "_proxy" `app`
  stringLit (localName resolvedName)
  where
    parseEnumFun :: Bool -> Text
    parseEnumFun True = "parseEnumNoUnknown"
    parseEnumFun False = "parseEnum"

genParseList :: HSType t -> Exp ()
genParseList ty = protocolFun "parseList" `app` genParseType P_ListMode ty

genParseMap :: HSType k -> HSType v -> Exp ()
genParseMap k v =
  protocolFun "parseMap" `app`
  genParseType P_ListMode k `app`
  genParseType P_ListMode v `app`
  qcon "Prelude" (if isStringType k then "True" else "False")

-- Generate NFData Instance ----------------------------------------------------

genNFData :: HS Struct -> HS.Decl ()
genNFData Struct{..} =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym "DeepSeq" "NFData")
       (TyCon () $ unqualSym structResolvedName))
    (Just $ map (InsDecl ())
     [ FunBind ()
       [ Match () (textToName "rnf")
         [ PApp () (unqualSym structResolvedName) $ map mkPattern structMembers ]
         (UnGuardedRhs () $
          foldr seqify (Con () (Special () (UnitCon ()))) structMembers)
         Nothing
       ]
     ])
    where
      seqify field = InfixApp () (forcedField field) infixSeq
      infixSeq = QVarOp () $ qualSym "Prelude" "seq"
      forcedField Field{..} =
        qvar "DeepSeq" "rnf" `app` var ("__field__" <> fieldName)

mkPattern :: HS (Field u) -> Pat ()
mkPattern Field{..} = pvar $ "__field__" <> fieldName

-- Generate Default Instance ---------------------------------------------------

genDefault :: HS Struct -> HS.Decl ()
genDefault Struct{..} =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym "Default" "Default")
       (TyCon () $ unqualSym structResolvedName))
    (Just $ map (InsDecl ())
     [ FunBind ()
       [ Match () (textToName "def") []
         (UnGuardedRhs () $
          foldl app (con structResolvedName) $ map genFieldDefault structMembers)
         Nothing
       ]
     ])

genFieldDefault :: HS (Field u) -> Exp ()
genFieldDefault Field{..} =
  case fieldRequiredness of
    Optional{} -> qcon "Prelude" "Nothing"
    _ -> case fieldResolvedVal of
           Just val -> genConst fieldResolvedType val
           Nothing  -> typeToDefault fieldResolvedType

-- Generate Hashable Instance --------------------------------------------------

genHashable :: HS Struct -> HS.Decl ()
genHashable Struct{..} =
  InstDecl () Nothing
  (IRule () Nothing Nothing $
   IHApp () (IHCon () (qualSym "Hashable" "Hashable")) $
   simpleType structResolvedName)
  (Just
   [ InsDecl () $ FunBind ()
     [ Match () (textToName "hashWithSalt")
       [ pvar "__salt"
       , PApp () (unqualSym structResolvedName) $
         map (\Field{..} -> pvar $ "_" <> fieldName) structMembers
       ]
       (UnGuardedRhs () $ foldl
        (\salt Field{..} ->
          qvar "Hashable" "hashWithSalt" `app`
          salt `app`
          transformValue mkHashable fieldRequiredness fieldResolvedType
          (var $ "_" <> fieldName))
        (var "__salt")
        structMembers)
       Nothing
     ]
   ])

-- Some thrift types aren't hashable, so we need to modify them before passing
-- them to hashWithSalt
mkHashable :: HSType t -> Maybe (Exp ())
mkHashable = \case
  I8 -> Nothing
  I16 -> Nothing
  I32 -> Nothing
  I64 -> Nothing
  (TSpecial HsInt) -> Nothing
  TFloat -> Nothing
  TDouble -> Nothing
  TText -> Nothing
  TBool -> Nothing
  (TSpecial HsString) -> Nothing
  (TSpecial HsByteString) -> Nothing
  TBytes -> Nothing
  -- elems gives the set elements in ascending order, so we do not need to
  -- sort the result
  (TSet u) -> Just $ mapTransform mkHashable u $ qvar "Set" "elems"
  (THashSet u) ->
    Just $ qvar "List" "sort" `compose`
    mapTransform mkHashable u (qvar "HashSet" "toList")
  (TList u) -> (qvar "Prelude" "map" `app`) <$> mkHashable u
  (TSpecial (HsVector vec u)) ->
    Just $ mapTransform mkHashable u $ qvar (hsVectorQual vec) "toList"
  (TMap k v) -> Just $ mapTransformPair mkHashable k v $ qvar "Map" "toAscList"
  (THashMap k v) ->
    Just $ qvar "List" "sort" `compose`
    mapTransformPair mkHashable k v (qvar "HashMap" "toList")
  TStruct{} -> Nothing
  TException{} -> Nothing
  TUnion{} -> Nothing
  TEnum{} -> Nothing
  (TTypedef _ u _loc) -> mkHashable u
  TNewtype{} -> Nothing

transformValue
  :: (HSType t -> Maybe (Exp ()))
  -> Requiredness u a
  -> HSType t
  -> Exp ()
  -> Exp ()
transformValue transform req ty exp =
  case (req, transform ty) of
    (_, Nothing) -> exp
    (Optional{}, Just f) -> qvar "Prelude" "fmap" `app` f `app` exp
    (_, Just f) -> f `app` exp

mapTransform
  :: (HSType t -> Maybe (Exp ())) -> HSType t -> Exp () -> Exp ()
mapTransform transform u =
  case transform u of
    Nothing -> id
    Just f -> compose $ qvar "Prelude" "map" `app` f

mapTransformPair
  :: (forall t. HSType t -> Maybe (Exp ()))
  -> HSType k
  -> HSType v
  -> Exp ()
  -> Exp ()
mapTransformPair transform k v = compose $
  qvar "Prelude" "map" `app`
  Lambda () [ PTuple () Boxed [ pvar "_k", pvar "_v" ] ]
  (Tuple () Boxed [ applyT transform k "_k", applyT transform v "_v" ])

applyT :: (forall u. HSType u -> Maybe (Exp ())) -> HSType t -> Text -> Exp ()
applyT transform u v =
  case transform u of
    Nothing -> var v
    Just f  -> f `app` var v

canDerive :: (forall t. HSType t -> Maybe a) -> HS Struct -> Bool
canDerive f Struct{..} =
  all (\Field{..} -> isNothing $ f fieldResolvedType) structMembers

-- Generate Hashable Instance --------------------------------------------------

genOrd :: HS Struct -> HS.Decl ()
genOrd Struct{..} =
  InstDecl () Nothing
  (IRule () Nothing Nothing $
   IHApp () (IHCon () (qualSym "Ord" "Ord")) $
   simpleType structResolvedName)
  (Just
   [ InsDecl () $ FunBind ()
     [ Match () (textToName "compare")
       [ pvar "__a"
       , pvar "__b"
       ]
       (UnGuardedRhs () $ case structMembers of
         [] -> qcon "Ord" "EQ"
         _:_ -> foldr1
           (\scrutinee continue ->
             Case () scrutinee
             [ Alt () (PApp () (qualSym "Ord" c) [])
               (UnGuardedRhs () result)
               Nothing
             | (c, result) <-
               [ ("LT", qcon "Ord" "LT")
               , ("GT", qcon "Ord" "GT")
               , ("EQ", continue)
               ]
             ])
           [ qvar "Ord" "compare" `app`
             f (var fieldResolvedName `app` var "__a") `app`
             f (var fieldResolvedName `app` var "__b")
           | Field{..} <- structMembers
           , let f = transformValue mkOrd fieldRequiredness fieldResolvedType
           ])
       Nothing
     ]
   ])

mkOrd :: HSType t -> Maybe (Exp ())
mkOrd = \case
  I8 -> Nothing
  I16 -> Nothing
  I32 -> Nothing
  I64 -> Nothing
  (TSpecial HsInt) -> Nothing
  TFloat -> Nothing
  TDouble -> Nothing
  TText -> Nothing
  TBool -> Nothing
  (TSpecial HsString) -> Nothing
  TBytes -> Nothing
  (TSpecial HsByteString) -> Nothing
  (TSet u) -> (qvar "Set" "map" `app`) <$> mkOrd u
  (THashSet u) ->
    Just $ qvar "List" "sort" `compose`
    mapTransform mkOrd u (qvar "HashSet" "toList")
  (TList u) -> (qvar "Prelude" "map" `app`) <$> mkOrd u
  (TSpecial (HsVector vec u)) ->
    (qvar (hsVectorQual vec) "map" `app`) <$> mkOrd u
  (TMap k v) -> case (mkOrd k, mkOrd v) of
    (Nothing, Nothing) -> Nothing
    _ -> Just $ mapTransformPair mkOrd k v $ qvar "Map" "toAscList"
  (THashMap k v) -> Just $ qvar "List" "sort" `compose`
    mapTransformPair mkOrd k v (qvar "HashMap" "toList")
  TStruct{} -> Nothing
  TException{} -> Nothing
  TUnion{} -> Nothing
  TEnum{} -> Nothing
  (TTypedef _ u _loc) -> mkOrd u
  TNewtype{} -> Nothing

canDeriveOrd :: HS Struct -> Bool
canDeriveOrd = canDerive mkOrd

-- Generate Exception Instance -------------------------------------------------

genException :: HS Struct -> HS.Decl ()
genException Struct{..} =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym "Exception" "Exception")
       (TyCon () $ unqualSym structResolvedName))
    Nothing

-- Generate Extra HasField Instances -------------------------------------------

genExtraHasFields :: HS Struct -> [HS.Decl ()]
genExtraHasFields Struct{..} = genHasField structResolvedName <$> structMembers

genHasField :: Text -> HS (Field u) -> HS.Decl ()
genHasField name field@Field{..} = InstDecl ()
  Nothing
  (IRule () Nothing Nothing ihead)
  (Just [InsDecl () getFieldDecl])
  where
    ihead =
      IHApp ()
        (IHApp ()
          (IHApp () (IHCon () $ qualSym "Thrift" "HasField") promotedName)
          (TyCon () $ unqualSym name)
        )
        (TyParen () $ genFieldType field)

    promotedName = promoteText fieldName

    getFieldDecl = FunBind () . (:[]) $
      Match ()
        (textToName "getField")
        mempty
        (UnGuardedRhs () (var fieldResolvedName))
        Nothing

promoteText :: Text -> HS.Type ()
promoteText s = TyPromoted ()
  $ PromotedString () fieldNameString fieldNameString
  where
    -- We need the thrift_ prefix in order to prevent naming conflicts between
    -- fields like struct.val and struct.struct_val
    fieldNameString = Text.unpack s
