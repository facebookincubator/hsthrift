-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
module Thrift.Compiler.Plugins.Haskell
  ( Haskell, HSType, HS
  , SpecialType(..)
  , HsVectorKind(..), hsVectorImport, hsVectorQual
  , HsInterface(..), RenameMap
  , LangOpts(..), defaultHsOpts
  , toCamel
  ) where

#if __GLASGOW_HASKELL__ > 804
#define This Some
#endif

import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import Data.Maybe
import Data.Some
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Type.Equality

import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Names hiding (None, resolve)
import qualified Language.Haskell.Exts.Syntax as E -- TODO: t16933748 refactor

import Thrift.Compiler.Options
import Thrift.Compiler.Parser
import Thrift.Compiler.Plugin
import Thrift.Compiler.Typechecker
import Thrift.Compiler.Typechecker.Monad
import Thrift.Compiler.Types as Thrift hiding (noLoc)

-- Haskell Types ---------------------------------------------------------------

data Haskell

type HSType = Type Haskell

type HS t = t 'Resolved Haskell Thrift.Loc

data HsVectorKind = HsVectorBoxed | HsVectorStorable
  deriving (Eq, Ord, Prelude.Enum, Bounded)

hsVectorImport :: HsVectorKind -> Text
hsVectorImport HsVectorBoxed = "Data.Vector"
hsVectorImport HsVectorStorable = "Data.Vector.Storable"

hsVectorQual :: HsVectorKind -> Text
hsVectorQual HsVectorBoxed = "Vector"
hsVectorQual HsVectorStorable = "VectorStorable"

data instance SpecialType Haskell t where
  HsInt    :: SpecialType Haskell Int
  HsString :: SpecialType Haskell String
  HsByteString :: SpecialType Haskell ByteString
  HsVector :: HsVectorKind -> HSType t -> SpecialType Haskell (List Haskell t)

data HsInterface = HsInterface Environment RenameMap

instance Semigroup HsInterface where
  (<>) = mappend

instance Monoid HsInterface where
  mempty = HsInterface Map.empty Map.empty
  mappend (HsInterface e1 r1) (HsInterface e2 r2) =
    HsInterface (Map.unionWith (++) e1 e2) (Map.union r1 r2)

-- | Map from Haskell name qualified Thrift name
type RenameMap = Map.Map Symbol Text

-- Haskell Options -------------------------------------------------------------

data instance LangOpts Haskell = HsOpts
  { hsoptsEnableHaddock :: Bool
  , hsoptsUseInt :: Bool
  , hsoptsUseHashMap :: Bool
  , hsoptsUseHashSet :: Bool
  , hsoptsDupNames :: Bool
  , hsoptsExtensions :: [Text]
  , hsoptsGenPrefix :: FilePath
  , hsoptsExtraHasFields :: Bool
  }

defaultHsOpts :: LangOpts Haskell
defaultHsOpts = HsOpts
  { hsoptsEnableHaddock = False
  , hsoptsUseInt = False
  , hsoptsUseHashMap = False
  , hsoptsUseHashSet = False
  , hsoptsDupNames = False
  , hsoptsExtensions = []
  , hsoptsGenPrefix = "gen-hs2"
  , hsoptsExtraHasFields = False
  }

-- Type Class Instance ---------------------------------------------------------

instance Typecheckable Haskell where
  type Interface Haskell = HsInterface

  -- Annotation Processing

  resolveTypeAnnotations ty anns = do
    Env{ options = Options{..} } <- ask
    case optsLangSpecific of
      HsOpts{..} ->
        ifFlag hsoptsUseInt i64ToInt .
        ifFlag hsoptsUseHashMap map2HashMap .
        ifFlag hsoptsUseHashSet set2HashSet <$>
        resolve ty (getTypeAnns "hs" anns)
    where
      resolve
        :: HSType t
        -> [(Text, Annotation Thrift.Loc)]
        -> TC Haskell (Some HSType)
      resolve I64 [("Int",_)] = special HsInt
      resolve TText [("String",_)] = special HsString
      resolve (TMap k v) [("HashMap",_)] = pure $ This $ THashMap k v
      resolve (TSet u) [("HashSet",_)] = pure $ This $ THashSet u
      resolve TText [("ByteString",_)] = special HsByteString
      resolve (TList u) [(vec,_)]
        | Just kind <-
            lookup vec [(hsVectorQual x,x) | x <- [minBound .. maxBound]] =
              special $ HsVector kind u
      resolve u [] = pure $ This u
      resolve u ((_,a):_) =
        typeError (annLoc a) $ AnnotationMismatch (AnnType u) a

      special = pure . This . TSpecial

      ifFlag
        :: Bool
        -> (forall t. HSType t -> Some HSType)
        -> Some HSType
        -> Some HSType
      ifFlag flag fun (This u)
        | flag      = fun u
        | otherwise = This u
      i64ToInt :: HSType t -> Some HSType
      i64ToInt I64 = This $ TSpecial HsInt
      i64ToInt u  = This u
      map2HashMap (TMap k v) = This $ THashMap k v
      map2HashMap u = This u
      set2HashSet (TSet u) = This $ THashSet u
      set2HashSet u = This u

  -- Typechecking

  qualifySpecialType _ HsInt = HsInt
  qualifySpecialType _ HsString = HsString
  qualifySpecialType _ HsByteString = HsByteString
  qualifySpecialType m (HsVector kind ty) = HsVector kind $ qualifyType m ty

  typecheckSpecialConst HsInt (UntypedConst _ (IntConst i _)) =
    pure $ Literal $ fromIntegral i
  typecheckSpecialConst HsString (UntypedConst _ (StringConst s _)) =
    pure $ Literal $ Text.unpack s
  typecheckSpecialConst HsByteString (UntypedConst _ (StringConst s _)) =
    pure $ Literal $ Text.encodeUtf8 s
  typecheckSpecialConst (HsVector _ u) (UntypedConst _ ListConst{..}) =
    Literal . List <$> mapT (typecheckConst u . leElem) lvElems
  typecheckSpecialConst ty val@(UntypedConst Located{..} _) =
    typeError lLocation $ LiteralMismatch (TSpecial ty) val

  eqSpecial HsInt HsInt = Just Refl
  eqSpecial HsString HsString = Just Refl
  eqSpecial HsByteString HsByteString = Just Refl
  eqSpecial (HsVector a u) (HsVector b v)
    | a == b = apply Refl <$> eqOrAlias u v
  eqSpecial _ _ = Nothing

  -- Interfaces

  getInterface opts tf@ThriftFile{..} = mconcat $
    map (getDeclIface opts thriftName mname) thriftDecls
    where
      mname = E.ModuleName () $ Text.unpack $ renameModule opts tf <> ".Types"

  getExtraSymbols opts iface tf@ThriftFile{..} =
    maybe [] (getHsIncludeDeps opts iface tf) thriftSplice

  -- Renamers

  renameModule _ ThriftFile{..} = case getNamespace "hs" thriftHeaders of
    Just ns -> ns <> "." <> toCamel thriftName
    Nothing -> toCamel thriftName

  renameStruct _ Struct{..} = uppercase structName

  renameField Options{..} ann sname Field{..} =
    case optsLangSpecific of
      HsOpts{..} ->
        let basePrefix
              | hsoptsDupNames = ""
              | otherwise = sname <> "_"
        in  lowercase $ fromMaybe basePrefix (getPrefix ann) <> fieldName

  renameConst _ = lowercase

  renameService _ Service{..} = uppercase serviceName

  renameFunction _ Function{..} = lowercase $ prefix <> funName
    where
      prefix = fromMaybe "" $ getPrefix $ getAnns funAnns

  renameTypedef _ Typedef{..} = uppercase tdName

  renameEnum _ Enum{..} = uppercase enumName

  renameEnumAlt opts@Options{..} e@Enum{..} name =
    fixCase $ if
      | Just prefix <- getPrefix (getAnns enumAnns) -> prefix <> name
      | otherwise -> enumName <> "_" <> name
    where
      fixCase
        | isPseudo opts e = lowercase
        | otherwise = uppercase

  renameUnion _ Union{..} = uppercase unionName

  renameUnionAlt _ Union{..} UnionAlt{..} =
    uppercase $ fromMaybe (unionName <> "_") (getPrefix $ getAnns unionAnns) <>
    altName

  getUnionEmptyName _ Union{..} =
    uppercase $ fromMaybe (unionName <> "_") (getPrefix $ getAnns unionAnns) <>
    "EMPTY"

  fieldsAreUnique Options{ optsLangSpecific = HsOpts{..} } = not hsoptsDupNames
  unionAltsAreUnique _ = True
  enumAltsAreUnique Options{..} = True

  isPseudo _ Enum{..} = or
    [ saTag == "hs.psuedoenum"
    | SimpleAnn{..} <- getAnns enumAnns
    ]

  -- Back-Translators

  backTranslateType HsInt = (This I64, "Int")
  backTranslateType HsString = (This TText, "String")
  backTranslateType HsByteString = (This TText, "ByteString")
  backTranslateType (HsVector kind u) = (This (TList u), hsVectorQual kind)

  backTranslateLiteral HsInt i = ThisLit I64 (fromIntegral i)
  backTranslateLiteral HsString s = ThisLit TText (Text.pack s)
  backTranslateLiteral HsByteString s = ThisLit TText (Text.decodeUtf8 s)
  backTranslateLiteral (HsVector _ u) l = ThisLit (TList u) l

-- Compute Decl Interfaces -----------------------------------------------------

getDeclIface
  :: Options Haskell -> Text -> E.ModuleName () -> Parsed Decl -> HsInterface
getDeclIface opts name mname decl = ifaceFromSymbols mname $ case decl of
  -- Structs
  D_Struct s@Struct{..} ->
    mkStruct (packT structName) (packHs $ renameStruct opts s) ++
    concatMap
    (\field ->
      mkSelector (packT structName)
      (packHs $ renameField opts (getAnns structAnns) structName field)
      (packHs $ renameStruct opts s))
    structMembers
  -- Unions
  D_Union u@Union{..} ->
    mkData (packT unionName) (packHs $ renameUnion opts u) ++
    concatMap
    (\alt ->
      mkConstructor (packT unionName)
      (packHs $ renameUnionAlt opts u alt)
      (packHs $ renameUnion opts u))
    unionAlts
  -- Enums
  D_Enum e@Enum{..}
    | isPseudo opts e ->
        mkNewtype (packT enumName) (packHs $ renameEnum opts e)
          (packHs $ ("un" <>) $ renameEnum opts e) ++
        concatMap
        (\EnumValue{..} ->
          mkValue (packT enumName) (packHs $ renameEnumAlt opts e evName))
        enumConstants
    | otherwise ->
        mkData (packT enumName) (packHs $ renameEnum opts e) ++
        concatMap
        (\EnumValue{..} ->
          mkConstructor (packT enumName)
          (packHs $ renameEnumAlt opts e evName)
          (packHs $ renameEnum opts e))
        enumConstants
  -- Typedefs
  D_Typedef t@Typedef{..}
    | isNewtype (getAnns tdAnns) ->
        mkNewtype (packT tdName) (packHs $ renameTypedef opts t)
        (packHs $ ("un" <>) $ renameTypedef opts t)
    | otherwise ->
        mkType (packT tdName) (packHs $ renameTypedef opts t)
  -- Constants
  D_Const Const{..} ->
    mkValue (packT constName) (packHs $ renameConst opts constName)
  -- Services are not suppoerted yet
  D_Service{} -> []

  where
    mkValue tname hsname =
      [ (Value mname hsname, tname) ]
    mkStruct tname hsname =
      [ (Data mname hsname, tname)
      , (Constructor mname hsname hsname, tname)
      ]
    mkSelector tname hsname tyname =
      [ (Selector mname hsname tyname [tyname], tname)
      ]
    mkData tname hsname =
      [ (Data mname hsname, tname) ]
    mkConstructor tname hsname tyname =
      [ (Constructor mname hsname tyname, tname) ]
    mkType tname hsname =
      [ (Type mname hsname, tname) ]
    mkNewtype tname hsname selname =
      [ (NewType mname hsname, tname)
      , (Constructor mname hsname hsname, tname)
      , (Selector mname selname hsname [hsname], tname)
      ]

    packHs = E.Ident () . Text.unpack
    packT t = name <> "." <> t

ifaceFromSymbols :: E.ModuleName () -> [(Symbol, Text)] -> HsInterface
ifaceFromSymbols mname ss = HsInterface
  (Map.singleton mname symbols)
  rmap
  where
    rmap = Map.fromList ss
    symbols = map fst ss

-- HS Include Dependencies -----------------------------------------------------

getHsIncludeDeps
  :: Options Haskell
  -> HsInterface
  -> ThriftFile a l
  -> E.Module SrcSpanInfo
  -> [Text]
getHsIncludeDeps opts (HsInterface env rmap) tf (E.Module loc mhead ps is ds) =
  [ thriftSym
  | decl <- decls
  , (Scoped (GlobalSymbol hsSymbol _) _) <- Foldable.toList decl
  , Just thriftSym <- [Map.lookup hsSymbol rmap]
  ]
  where
    E.Module _ _ _ _ decls = annotate env m'

    -- Add types module to imports so that haskell-names knows where the symbols
    -- come from
    m' = E.Module loc mhead ps (thriftImport : is) ds
    thriftImport = E.ImportDecl
      { importAnn = emptyLoc
      , importModule =
        E.ModuleName emptyLoc $ Text.unpack (renameModule opts tf) ++ ".Types"
      , importQualified = False
      , importSrc = False
      , importSafe = False
      , importPkg = Nothing
      , importAs = Nothing
      , importSpecs = Nothing
      }
    emptyLoc = toSrcInfo noLoc [] noLoc
getHsIncludeDeps _ _ _ _ = []
