-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
module Thrift.Compiler.GenUtils
  ( textToName
  , qualSym, unqualSym, nameToQName
  , qualType, simpleType
  , qvar, var, pvar, con, qcon, tvar
  , app, appT, infixApp, compose
  , genImportModule, importFromInclude, typeToImport
  , intLit, stringLit, intP, stringP, listE
  , deriving_
  , genType, isBaseType, genThriftType, genConst, typeToDefault
  , qualifyType, qualifyField, genConstructor
  , Import(..)
  , protocolFun
  , genCALL, genREPLY, genEXCEPTION, genONEWAY
  , PrimType(..), getPrim
  ) where

#if __GLASGOW_HASKELL__ > 804
#define This Some
#endif

import Data.Proxy
import Data.Set (union)
import Data.Some
import Data.Text (Text)
import Data.Text.Encoding hiding (Some)
import GHC.TypeLits
import Language.Haskell.Exts.Syntax hiding (List, Name, Type)
import qualified Language.Haskell.Exts.Syntax as HS
import qualified Data.Set as Set
import qualified Data.Text as Text

import Thrift.Compiler.Plugins.Haskell
import Thrift.Compiler.Types

data Import
   = QImport Text Text
   | SymImport Text [Text]
   | TypesImport Text
   deriving (Show, Eq, Ord)

textToName :: Text -> HS.Name ()
textToName = Ident () . Text.unpack

genImportModule :: Import -> ImportDecl ()
genImportModule (QImport modName modAs) = ImportDecl
  { importAnn       = ()
  , importModule    = ModuleName () $ Text.unpack modName
  , importQualified = True
  , importSrc       = False
  , importSafe      = False
  , importPkg       = Nothing
  , importAs        = Just . ModuleName () $ Text.unpack modAs
  , importSpecs     = Nothing
  }
genImportModule (SymImport modName symbols) = ImportDecl
  { importAnn       = ()
  , importModule    = ModuleName () $ Text.unpack modName
  -- Symbols are not imported qualified because you can't define your own
  -- operators in Thrift, therefore they are guaranteed not to collide
  , importQualified = False
  , importSrc       = False
  , importSafe      = False
  , importPkg       = Nothing
  , importAs        = Nothing
  , importSpecs     = Just $ ImportSpecList () False (map mkImport symbols)
  }
    where mkImport = IVar () . Symbol () . Text.unpack
genImportModule (TypesImport modName) = ImportDecl
  { importAnn       = ()
  , importModule    = ModuleName () $ Text.unpack modName ++ ".Types"
  , importQualified = False
  , importSrc       = False
  , importSafe      = False
  , importPkg       = Nothing
  , importAs        = Nothing
  , importSpecs     = Nothing
  }

importFromInclude :: Program Haskell a -> Import
importFromInclude Program{..} =
  QImport (progHSName <> ".Types") progHSName

typeToImport :: HSType t -> Set.Set Import
typeToImport I8  = Set.singleton (QImport "Data.Int" "Int")
typeToImport I16 = Set.singleton (QImport "Data.Int" "Int")
typeToImport I32 = Set.singleton (QImport "Data.Int" "Int")
typeToImport I64 = Set.singleton (QImport "Data.Int" "Int")
typeToImport (TSpecial HsInt) = Set.empty
typeToImport TFloat = Set.empty
typeToImport TDouble = Set.empty
typeToImport TText   = Set.fromList
                       [ QImport "Data.Text" "Text"
                       , QImport "Data.Text.Encoding" "Text"
                       ]
typeToImport (TSpecial HsString) = typeToImport TText
typeToImport (TSpecial HsByteString) = Set.fromList
  [ QImport "Data.ByteString" "ByteString"
  , QImport "Data.Text" "Text"
  , QImport "Data.Text.Encoding" "Text"
  ]
typeToImport TBytes  = Set.fromList
  [ QImport "Data.ByteString" "ByteString"
  ]
typeToImport TBool   = Set.empty
typeToImport (TSet t) =
  Set.singleton (QImport "Data.Set" "Set") `union`
  typeToImport t
typeToImport (THashSet t) =
  Set.singleton (QImport "Data.HashSet" "HashSet") `union`
  typeToImport t
typeToImport (TList t) = typeToImport t
typeToImport (TSpecial (HsVector vec t)) =
  Set.singleton (QImport (hsVectorImport vec) (hsVectorQual vec)) `union`
  typeToImport t
typeToImport (TMap k v) =
  Set.singleton (QImport "Data.Map.Strict" "Map") `union`
  typeToImport k `union`
  typeToImport v
typeToImport (THashMap k v) =
  Set.singleton (QImport "Data.HashMap.Strict" "HashMap") `union`
  typeToImport k `union`
  typeToImport v
typeToImport (TStruct name _loc) = nameToImport name
typeToImport (TException name _loc) = nameToImport name
typeToImport (TUnion name _loc) = nameToImport name
typeToImport (TEnum name _loc _) = nameToImport name
typeToImport (TTypedef name ty _loc) = nameToImport name `union` typeToImport ty
typeToImport (TNewtype name ty _loc) = nameToImport name `union` typeToImport ty

nameToImport :: Name -> Set.Set Import
nameToImport Name{..} = case resolvedName of
  UName{} -> Set.empty
  QName q _ -> Set.singleton $ QImport (q <> ".Types") q

unqualSym :: Text -> QName ()
unqualSym = UnQual () . textToName

qualSym :: Text -> Text -> QName ()
qualSym m sym = Qual () (ModuleName () $ Text.unpack m) (textToName sym)

nameToQName :: Name -> QName ()
nameToQName Name{..} = case resolvedName of
  UName n -> unqualSym n
  QName m n -> qualSym m n

qualType :: Text -> Text -> HS.Type ()
qualType mname tname = TyCon () $ qualSym mname tname

simpleType :: Text -> HS.Type ()
simpleType tname = TyCon () $ unqualSym tname

app :: Exp () -> Exp () -> Exp ()
app = App ()
infixl `app`

appT :: HS.Type () -> HS.Type () -> HS.Type ()
appT = TyApp ()
infixl `appT`

infixApp :: Text -> Exp () -> Exp () -> Exp ()
infixApp sym e1 e2 =
  InfixApp () e1 (QVarOp () $ UnQual () $ Symbol () $ Text.unpack sym) e2

compose :: Exp () -> Exp () -> Exp ()
compose = infixApp "."

stringLit :: Text -> Exp ()
stringLit text = Lit () $ String () s s
  where s = Text.unpack text

listE :: [Exp ()] -> Exp ()
listE = HS.List ()

intLit :: (Integral a, Show a) => a -> Exp ()
intLit x = wrapNegativeLit x $ Lit () $ Int () (fromIntegral x) (show x)

floatLit :: (Real a, Show a) => a -> Exp ()
floatLit x = wrapNegativeLit x $ Lit () $ Frac () (toRational x) (show x)

wrapNegativeLit :: Show a => a -> Exp () -> Exp ()
wrapNegativeLit x = case show x of
  '-':_ -> Paren ()
  _ -> id
  -- NB: we check the result of show directly to handle negative zeros
  --
  -- >>> let x = -0.0 in (show x, signum x, x < 0)
  -- ("-0.0", -0.0, False)

intP :: Int -> Pat ()
intP x = PLit () (sign ()) $ Int () (fromIntegral $ abs x) (show x)
  where sign | x < 0 = Negative
             | otherwise = Signless

stringP :: Text -> Pat ()
stringP text = PLit () (Signless ()) $ String () s s
  where s = Text.unpack text

var :: Text -> Exp ()
var = Var () . unqualSym

qvar :: Text -> Text -> Exp ()
qvar m n = Var () $ qualSym m n

pvar :: Text -> Pat ()
pvar name = PVar () $ textToName name

con :: Text -> Exp ()
con = Con () . unqualSym

qcon :: Text -> Text -> Exp ()
qcon m n = Con () $ qualSym m n

tvar :: Text -> HS.Type ()
tvar = TyVar () . textToName

deriving_ :: [InstRule ()] -> Deriving ()
deriving_ = Deriving () Nothing

genType :: HSType t -> HS.Type ()
genType I8      = qualType "Int" "Int8"
genType I16     = qualType "Int" "Int16"
genType I32     = qualType "Int" "Int32"
genType I64     = qualType "Int" "Int64"
genType (TSpecial HsInt) = qualType "Prelude" "Int"
genType TFloat  = qualType "Prelude" "Float"
genType TDouble = qualType "Prelude" "Double"
genType TBool   = qualType "Prelude" "Bool"
genType TText   = qualType "Text" "Text"
genType (TSpecial HsString) = qualType "Prelude" "String"
genType (TSpecial HsByteString) = qualType "ByteString" "ByteString"
genType TBytes  = qualType "ByteString" "ByteString"
genType (TList ty) = TyList () (genType ty)
genType (TSpecial (HsVector vec ty)) =
  qualType (hsVectorQual vec) "Vector" `appT` genType ty
genType (TSet ty)  = qualType "Set" "Set" `appT` genType ty
genType (THashSet ty) = qualType "HashSet" "HashSet" `appT` genType ty
genType (TMap k v) = qualType "Map" "Map" `appT` genType k `appT` genType v
genType (THashMap k v) =
  qualType "HashMap" "HashMap" `appT` genType k `appT` genType v
genType (TStruct name _loc)    = TyCon () $ nameToQName name
genType (TException name _loc) = TyCon () $ nameToQName name
genType (TUnion name _loc)     = TyCon () $ nameToQName name
genType (TEnum name _loc _)      = TyCon () $ nameToQName name
genType (TTypedef name _ _loc) = TyCon () $ nameToQName name
genType (TNewtype name _ _loc) = TyCon () $ nameToQName name

genConst :: HSType t -> TypedConst Haskell t -> Exp ()
-- Base Type Literals
genConst I8  (Literal x) = intLit x
genConst I16 (Literal x) = intLit x
genConst I32 (Literal x) = intLit x
genConst I64 (Literal x) = intLit x
genConst (TSpecial HsInt) (Literal x) = intLit x
genConst TFloat  (Literal x) = floatLit x
genConst TDouble (Literal x) = floatLit x
genConst TBool   (Literal x) = qvar "Prelude" $ if x then "True" else "False"
genConst TText   (Literal s) = stringLit s
genConst (TSpecial HsString) (Literal s) = Lit () $ String () s s
genConst (TSpecial HsByteString) (Literal s) = stringLit $ decodeUtf8 s
genConst TBytes (Literal s) = stringLit $ decodeUtf8 s
-- Collection Literals
genConst (TList ty)   (Literal (List l)) = HS.List () $ map (genConst ty) l
genConst (TSpecial (HsVector vec ty)) (Literal (List l)) =
  qvar (hsVectorQual vec) "fromList" `app` HS.List () (map (genConst ty) l)
genConst (TSet ty) (Literal (Set l)) =
  qvar "Set" "fromList" `app` HS.List () (map (genConst ty) l)
genConst (THashSet ty) (Literal (HashSet l)) =
  qvar "HashSet" "fromList" `app` HS.List () (map (genConst ty) l)
genConst (TMap kt vt) (Literal (Map m)) =
  qvar "Map" "fromList" `app`
  HS.List () (map (\(k, v) -> Tuple () Boxed [genConst kt k, genConst vt v]) m)
genConst (THashMap kt vt) (Literal (HashMap m)) =
  qvar "HashMap" "fromList" `app`
  HS.List () (map (\(k, v) -> Tuple () Boxed [genConst kt k, genConst vt v]) m)
-- Names Type Constants
genConst (TTypedef _ ty _loc) lit@Literal{} = genConst ty lit
genConst (TNewtype name ty _loc) (Literal (New lit)) =
  Con () (nameToQName name) `app` genConst ty (Literal lit)
genConst (TStruct name _loc) (Literal (This s)) = genStructConst name s
genConst (TException name _loc) (Literal (This (EV s))) = genStructConst name s
genConst (TUnion name _loc) (Literal (This (UnionVal proxy ty val _))) =
  Con () (nameToQName cname) `app` genConst ty val
  where
    conName = Text.pack $ symbolVal proxy
    cname = name { resolvedName = mapName (const conName) $ resolvedName name }
genConst (TEnum _ _loc _) (Literal (EnumVal name _)) = Con () $ nameToQName name
-- Identifiers
genConst _ (Identifier name _ _loc) = Var () $ nameToQName name
-- TODO: WeirdEnumToInt needs some kind of explicit conversion, this should fail
genConst _ (WeirdEnumToInt _ name _ _loc) = Var () $ nameToQName name

genStructConst :: Name -> StructVal Haskell s -> Exp ()
genStructConst name struct =
  case fields struct of
    [] -> def
    flds -> RecUpdate () (Paren () def) flds
  where
    def = ExpTypeSig () (qvar "Default" "def") (TyCon () $ nameToQName name)
    fields :: StructVal Haskell s -> [FieldUpdate ()]
    fields Empty = []
    fields (ConsVal proxy ty val s) =
      FieldUpdate () (getName proxy) (genConst ty val) : fields s
    fields (ConsDefault _ _ s) = fields s
    fields (ConsJust proxy ty val s) =
      FieldUpdate () (getName proxy)
        (qcon "Prelude" "Just" `app` genConst ty val) : fields s
    fields (ConsNothing _ s) = fields s
    getName :: KnownSymbol s => Proxy s -> QName ()
    getName proxy =
      nameToQName name { resolvedName = mapName (const n) $ resolvedName name }
      where n = Text.pack $ symbolVal proxy

typeToDefault :: HSType t -> Exp ()
typeToDefault TBool = qcon "Prelude" "False"
typeToDefault TText = stringLit ""
typeToDefault TBytes = stringLit ""
typeToDefault THashMap{} = qvar "HashMap" "empty"
typeToDefault THashSet{} = qvar "HashSet" "empty"
typeToDefault (TSpecial (HsVector vec _)) = qvar (hsVectorQual vec) "empty"
typeToDefault (TTypedef _ ty _loc) = typeToDefault ty
typeToDefault (TNewtype name ty _loc) =
  Con () (nameToQName name) `app` typeToDefault ty
-- Everything else has default instances
typeToDefault _ = qvar "Default" "def"

isBaseType :: HSType t -> Bool
isBaseType I8 = True
isBaseType I16 = True
isBaseType I32 = True
isBaseType I64 = True
isBaseType (TSpecial HsInt) = True
isBaseType TFloat = True
isBaseType TDouble = True
isBaseType TBool = True
isBaseType TText = True
isBaseType (TSpecial HsString) = True
isBaseType (TSpecial HsByteString) = True
isBaseType TBytes = True
isBaseType (TTypedef _ t _loc) = isBaseType t
isBaseType (TNewtype _ t _loc) = isBaseType t
isBaseType _ = False

genThriftType :: HSType t -> Exp ()
genThriftType I8      = protocolFun "getByteType"
genThriftType I16     = protocolFun "getI16Type"
genThriftType I32     = protocolFun "getI32Type"
genThriftType I64     = protocolFun "getI64Type"
genThriftType (TSpecial HsInt) = protocolFun "getI64Type"
genThriftType TFloat  = protocolFun "getFloatType"
genThriftType TDouble = protocolFun "getDoubleType"
genThriftType TBool   = protocolFun "getBoolType"
genThriftType TText   = protocolFun "getStringType"
genThriftType (TSpecial HsString) = protocolFun "getStringType"
genThriftType (TSpecial HsByteString) = protocolFun "getStringType"
genThriftType TBytes  = protocolFun "getStringType"
genThriftType TList{}    = protocolFun "getListType"
genThriftType (TSpecial HsVector{}) = protocolFun "getListType"
genThriftType TSet{}     = protocolFun "getSetType"
genThriftType THashSet{} = protocolFun "getSetType"
genThriftType TMap{}     = protocolFun "getMapType"
genThriftType THashMap{} = protocolFun "getMapType"
genThriftType TStruct{}  = protocolFun "getStructType"
genThriftType TException{} = protocolFun "getStructType"
genThriftType TUnion{} = protocolFun "getStructType"
genThriftType TEnum{}    = protocolFun "getI32Type"
genThriftType (TTypedef _ ty _loc) = genThriftType ty
genThriftType (TNewtype _ ty _loc) = genThriftType ty

protocolFun :: Text -> Exp ()
protocolFun fun = qvar "Thrift" fun `app` var "_proxy"

genCALL, genREPLY, genEXCEPTION, genONEWAY :: Int
genCALL      = 1
genREPLY     = 2
genEXCEPTION = 3
genONEWAY    = 4

data PrimType = P_I8 | P_I16 | P_I32 | P_I64 | P_Bool

getPrim :: HSType t -> Maybe PrimType
getPrim I8    = Just P_I8
getPrim I16   = Just P_I16
getPrim I32   = Just P_I32
getPrim I64   = Just P_I64
getPrim TBool = Just P_Bool
getPrim _ = Nothing

qualifyType :: Text -> HSType t -> HSType t
qualifyType _ I8      = I8
qualifyType _ I16     = I16
qualifyType _ I32     = I32
qualifyType _ I64     = I64
qualifyType _ (TSpecial HsInt) = TSpecial HsInt
qualifyType _ TFloat  = TFloat
qualifyType _ TDouble = TDouble
qualifyType _ TBool   = TBool
qualifyType _ TText   = TText
qualifyType _ (TSpecial HsString) = TSpecial HsString
qualifyType _ (TSpecial HsByteString) = TSpecial HsByteString
qualifyType _ TBytes  = TBytes
qualifyType q (TList u) = TList $ qualifyType q u
qualifyType q (TSpecial (HsVector vec u)) =
  TSpecial $ HsVector vec $ qualifyType q u
qualifyType q (TSet u) = TSet $ qualifyType q u
qualifyType q (THashSet u) = THashSet $ qualifyType q u
qualifyType q (TMap k v) = TMap (qualifyType q k) (qualifyType q v)
qualifyType q (THashMap k v) = THashMap (qualifyType q k) (qualifyType q v)
qualifyType q (TStruct name loc) = TStruct (qualifyName q name) loc
qualifyType q (TException name loc) = TException (qualifyName q name) loc
qualifyType q (TEnum name loc nounknown) =
  TEnum (qualifyName q name) loc nounknown
qualifyType q (TUnion name loc) = TUnion (qualifyName q name) loc
qualifyType q (TTypedef name ty loc) =
  TTypedef (qualifyName q name) (qualifyType q ty) loc
qualifyType q (TNewtype name ty loc) =
  TNewtype (qualifyName q name) (qualifyType q ty) loc

qualifyField
  :: Text -> Field u 'Resolved Haskell a -> Field u 'Resolved Haskell a
qualifyField q Field{..} = Field
  { fieldResolvedType = qualifyType q fieldResolvedType
  , fieldResolvedVal  = qualifyConst q fieldResolvedType <$> fieldResolvedVal
  , ..
  }

qualifyConst :: Text -> HSType t -> TypedConst Haskell t -> TypedConst Haskell t
qualifyConst q _ (Identifier name ty loc) =
  Identifier (qualifyName q name) (qualifyType q ty) loc
qualifyConst q _ (WeirdEnumToInt ty name tEnum loc) =
  WeirdEnumToInt (qualifyType q ty)
    (qualifyName q name) (qualifyType q tEnum) loc
qualifyConst q ty (Literal lit) = Literal $ qualifyLit q ty lit

qualifyLit :: Text -> HSType t -> t -> t
-- Base types don't need to be qualified
qualifyLit _ I8  x = x
qualifyLit _ I16 x = x
qualifyLit _ I32 x = x
qualifyLit _ I64 x = x
qualifyLit _ (TSpecial HsInt) x = x
qualifyLit _ TFloat f = f
qualifyLit _ TDouble d = d
qualifyLit _ TBool b = b
qualifyLit _ TText t = t
qualifyLit _ TBytes t = t
qualifyLit _ (TSpecial HsString) s = s
qualifyLit _ (TSpecial HsByteString) s = s
-- Collections
qualifyLit q (TList u) (List l) = List $ map (qualifyConst q u) l
qualifyLit q (TSpecial (HsVector _ u)) (List l) =
  List $ map (qualifyConst q u) l
qualifyLit q (TSet u) (Set s) = Set $ map (qualifyConst q u) s
qualifyLit q (THashSet u) (HashSet s) = HashSet $ map (qualifyConst q u) s
qualifyLit q (TMap kt vt) (Map m) =
  Map [ (qualifyConst q kt k, qualifyConst q vt v) | (k, v) <- m ]
qualifyLit q (THashMap kt vt) (HashMap m) =
  HashMap [ (qualifyConst q kt k, qualifyConst q vt v) | (k, v) <- m ]
-- Named Types
qualifyLit q TStruct{} (This s) = This $ qualifyStruct q s
qualifyLit q TException{} (This (EV s)) = This $ EV $ qualifyStruct q s
qualifyLit q TEnum{} (EnumVal name loc) = EnumVal (qualifyName q name) loc
qualifyLit q TUnion{} (This u) = This $ qualifyUnion q u
qualifyLit q (TTypedef _ ty _loc) lit = qualifyLit q ty lit
qualifyLit q (TNewtype _ ty _loc) (New lit) = New $ qualifyLit q ty lit

qualifyStruct :: Text -> StructVal Haskell s -> StructVal Haskell s
qualifyStruct _ Empty = Empty
qualifyStruct q (ConsVal pr ty val s) =
  ConsVal pr (qualifyType q ty) (qualifyConst q ty val) $ qualifyStruct q s
qualifyStruct q (ConsDefault pr ty s) =
  ConsDefault pr (qualifyType q ty) $ qualifyStruct q s
qualifyStruct q (ConsJust pr ty val s) =
  ConsJust pr (qualifyType q ty) (qualifyConst q ty val) $
  qualifyStruct q s
qualifyStruct q (ConsNothing pr s) = ConsNothing pr $ qualifyStruct q s

qualifyUnion :: Text -> UnionVal Haskell s -> UnionVal Haskell s
qualifyUnion q (UnionVal pr ty val proof) =
  UnionVal pr (qualifyType q ty) (qualifyConst q ty val) proof

-- Note: we only care about qualifying the resolved name because the source name
-- is not used anymore after typechecking
qualifyName :: Text -> Name -> Name
qualifyName q name@Name{..} = name
  { resolvedName = case resolvedName of
      UName n -> QName q n
      n@QName{} -> n
  }

genConstructor
  :: Maybe Text -> HSType (Some (ExceptionVal Haskell)) -> QName ()
genConstructor qual (TException name _loc)
  | Just uname <- getUName name = case qual of
      Nothing -> unqualSym uname
      Just q  -> qualSym q uname
genConstructor _ (TException name _loc) = nameToQName name
genConstructor qual (TTypedef _ ty _loc) = genConstructor qual ty
-- This case is impossible since there is no possible value for
-- SpecialType Haskell (Some (ExceptionVal Haskell))
-- , which is obvious from looking at the data family instance.
-- However, the pattern match completeness checker for HSType
-- doesn't seem to be able to eliminate this case.
genConstructor _ (TSpecial _) = error "This is provably unreachable"

getUName :: Name -> Maybe Text
getUName Name{..} = case resolvedName of
  UName n -> Just n
  QName{} -> Nothing
