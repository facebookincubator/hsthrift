{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeOperators, NamedFieldPuns, ApplicativeDo #-}
module Thrift.Compiler.Typechecker
  ( typecheck
  , typecheckConst, eqOrAlias
  , PartitionedDecls(..), partitionDecls
  , ModuleMap, sortModules
  ) where

import Prelude hiding (Enum)
import Data.List hiding (uncons)
import Data.Maybe
import Data.Some
import Data.Text.Encoding hiding (Some)
import Data.Type.Equality
import Control.Monad
import Data.Graph
import Data.Text (Text)
import GHC.TypeLits hiding (TypeError)
import GHC.Float
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Thrift.Compiler.Options
import Thrift.Compiler.Parser
import Thrift.Compiler.Plugin
import Thrift.Compiler.Typechecker.Monad
import Thrift.Compiler.Types

data PartitionedDecls s l a = Decls
  { dTdefs   :: [Typedef s l a]
  , dStructs :: [Struct s l a]
  , dUnions  :: [Union s l a]
  , dEnums   :: [Enum s l a]
  , dConsts  :: [Const s l a]
  , dServs   :: [Service s l a]
  , dInteractions   :: [Interaction s l a]
  }

partitionDecls :: [Decl s l a] -> PartitionedDecls s l a
partitionDecls = foldr addDecl emptyDecls
  where
    addDecl (D_Typedef t)  decls@Decls{..} = decls { dTdefs   = t : dTdefs   }
    addDecl (D_Struct s)   decls@Decls{..} = decls { dStructs = s : dStructs }
    addDecl (D_Union u)    decls@Decls{..} = decls { dUnions  = u : dUnions  }
    addDecl (D_Enum e)     decls@Decls{..} = decls { dEnums   = e : dEnums   }
    addDecl (D_Const c)    decls@Decls{..} = decls { dConsts  = c : dConsts  }
    addDecl (D_Service s)  decls@Decls{..} = decls { dServs   = s : dServs   }
    addDecl (D_Interaction s) decls@Decls{..} = decls { dInteractions = s : dInteractions }

emptyDecls :: PartitionedDecls s l a
emptyDecls = Decls
  { dTdefs   = []
  , dStructs = []
  , dUnions  = []
  , dEnums   = []
  , dConsts  = []
  , dServs   = []
  , dInteractions = []
  }

getLoc :: Parsed Decl -> Loc
getLoc (D_Typedef Typedef{..}) = lLocation $ tdlName tdLoc
getLoc (D_Struct Struct{..}) = lLocation $ slName structLoc
getLoc (D_Union Union{..}) = lLocation $ slName unionLoc
getLoc (D_Enum Enum{..}) = lLocation $ slName enumLoc
getLoc (D_Const Const{..}) = lLocation $ clName constLoc
getLoc (D_Service Service{..}) = lLocation $ slName serviceLoc
getLoc (D_Interaction Interaction{..}) = lLocation $ slName interactionLoc

-- Main Typechecking Function --------------------------------------------------

-- Given a map containing the tree of modules, typecheck all the things
-- We do a left fold over the modules, so in the end they are reverse
-- topologically sorted; the original module is the head of the list
typecheck
  :: Typecheckable l
  => Options l
  -> ModuleMap
  -> Either [TypeError l] (Program l Loc, [Program l Loc])
typecheck opts =
  uncons <=< foldM (typecheckModule opts) [] <=< sortAndPrune
    where
      sortAndPrune = fmap (pruneModules opts) . sortModules
      uncons (p : ps) = pure (p, ps)
      uncons []       = Left [EmptyInput]

typecheckModule
   :: Typecheckable l
   => Options l
   -> [Program l Loc]
   -> ThriftFile SpliceFile Loc
   -> Either [TypeError l] [Program l Loc]
typecheckModule opts@Options{..} progs tf@ThriftFile{..} = do
  -- Make Imports Map
  let
    imports   = [ incPath | HInclude{incType=Include,..} <- thriftHeaders ]
    includes  = [ prog | prog@Program{..} <- progs
                , optsLenient || progPath `elem` imports ]
    importMap = mkImportMap opts progs imports
    renamedModule = renameModule opts tf
    thriftDeclsNew
      | optsLenient && Map.notMember thriftName importMap =
          unSelfQualDecls thriftName thriftDecls
      | otherwise = thriftDecls
  -- Make Type Map
  tmap <- mkTypemap (thriftName, opts) importMap thriftDeclsNew
  -- Make Schema, Constants, and Services Maps
  let
    Decls{..} = partitionDecls thriftDeclsNew
    emap = mkEnumMap opts dEnums
    imap = mkEnumInt opts dEnums
  (smap, umap, cmap, servMap)
     <- (,,,) <$> mkSchemaMap (thriftName, opts) importMap tmap dStructs
         <*> mkUnionMap (thriftName, opts) importMap tmap dUnions
         <*> mkConstMap (thriftName, opts) importMap tmap thriftDeclsNew
         <*> mkServiceMap (thriftName, opts) importMap dServs

  -- Build the Env
  let env = Env { typeMap    = tmap
                , schemaMap  = smap
                , unionMap   = umap
                , enumMap    = emap
                , enumInt    = imap
                , constMap   = cmap
                , serviceMap = servMap
                , importMap  = importMap
                , options    = opts
                , envName    = thriftName -- for weird 'mkThriftName' case
                }
  -- Typecheck the rest of the things
  headers <- runTypechecker env $ traverse typecheckHeader thriftHeaders
  decls <- runTypechecker env $ traverse resolveDecl thriftDeclsNew
  let prog = Program
             { progName      = thriftName
             , progHSName    = renamedModule
             , progPath      = thriftPath
             , progOutPath   = optsOutPath
             , progInstances = thriftSplice
             , progIncludes  = includes
             , progHeaders   = headers
             , progDecls     = decls
             , progComments  = thriftComments
             , progEnv       = qualify (thriftName, renamedModule) env
             }
  return $ prog : progs

typecheckHeader
  :: Typecheckable l
  => Header 'Unresolved () Loc
  -> TC l (Header 'Resolved  l Loc)
typecheckHeader HInclude{..} = pure $ HInclude{..}
typecheckHeader HNamespace{..} = pure $ HNamespace{..}
typecheckHeader HPackage{..} = do
  sAnns <- resolveStructuredAnns pkgSAnns
  pure $ HPackage{pkgSAnns = sAnns, ..}

-- Self qualified --------------------------------------------------------------

-- | Takes the current thriftName (envName, progName) and removes this
-- from any 'TNamed' type that uses this thriftName a qualifier, i.e.
-- removes the qualifier if it is for the current thrift module.
--
-- We need to do this early, otherwise building the type map fails.
--
-- This is only used in @--lenient@ mode.
unSelfQualDecls :: Text -> [Parsed Decl] -> [Parsed Decl]
unSelfQualDecls thriftName xs = map unSelfQual xs
  where
    self = thriftName <> "."

    unSelfQual :: Parsed Decl -> Parsed Decl
    unSelfQual p = case p of
      D_Struct Struct{..}
        -> D_Struct Struct{structMembers = map usqField structMembers, ..}
      D_Union Union{..} -> D_Union Union{unionAlts = map usqAlt unionAlts, ..}
      D_Const Const{..}
        | Just x <- usqAnnType constType -> D_Const Const{constType=x, ..}
      D_Typedef Typedef{..}
        | Just x <- usqAnnType tdType -> D_Typedef Typedef{tdType=x, ..}
      D_Service Service{..}
        -> D_Service Service{serviceStmts = map usqStmt serviceStmts, ..}
      _ -> p

    usqField f@Field{..}
      | Just x <- usqAnnType fieldType = Field{fieldType=x, ..}
      | otherwise = f

    usqAlt :: UnionAlt 'Unresolved () Loc -> UnionAlt 'Unresolved () Loc
    usqAlt a@UnionAlt{..}
      | Just x <- usqAnnType altType = UnionAlt{altType=x, ..}
      | otherwise = a

    usqStmt :: ServiceStmt 'Unresolved () Loc -> ServiceStmt 'Unresolved () Loc
    usqStmt (FunctionStmt Function{..}) = FunctionStmt $ Function
      { funType = case funType of
          FunType (Some at) | Just x <- usqAnnType at -> FunType (Some x)
          FunTypeResponseAndStreamReturn ResponseAndStreamReturn{..}
            | Stream{..} <- rsStream
            , Just x <- usqAnnType streamType ->
                FunTypeResponseAndStreamReturn $ ResponseAndStreamReturn
                  { rsStream = Stream{streamType=x, ..}
                  , ..
                  }
          _ -> funType
      , funArgs = map usqField funArgs
      , funExceptions = map usqField funExceptions
      , .. }
    usqStmt (PerformsStmt p) = PerformsStmt p

    usqAnnType :: forall v. AnnotatedType Loc v -> Maybe (AnnotatedType Loc v)
    usqAnnType t@AnnotatedType{..} = (\x -> t{atType=x}) <$> usqType atType

    usqType
      :: forall v. TType 'Unresolved () Loc v
      -> Maybe (TType 'Unresolved () Loc v)
    usqType (TSet t) | Just x <- usqAnnType t = Just $ TSet x
    usqType (THashSet t) | Just x <- usqAnnType t = Just $ THashSet x
    usqType (TList t) | Just x <- usqAnnType t = Just $ TList x
    usqType (TMap k v) = case (usqAnnType k, usqAnnType v) of
      (Just x, Just y) -> Just $ TMap x y
      (Just x, _) -> Just $ TMap x v
      (_, Just y) -> Just $ TMap k y
      _ -> Nothing
    usqType (THashMap k v) = case (usqAnnType k, usqAnnType v) of
      (Just x, Just y) -> Just $ THashMap x y
      (Just x, _) -> Just $ THashMap x v
      (_, Just y) -> Just $ THashMap k y
      _ -> Nothing
    usqType (TNamed input) | Just x <- Text.stripPrefix self input =
      Just (TNamed x) -- Here is the point of unSelfQualDecls
    usqType _ = Nothing

-- Map Builders ----------------------------------------------------------------

-- | When using @--lenient@ mode, this includes not only the direct import but
-- also all others.  If there is a naming collision then direct wins over
-- indirect, but naming collisions between indirect ones is resolved in an
-- unspecified way. See T43181464 for weird examples of transitive imports.
mkImportMap :: Options l -> [Program l a] -> [FilePath] -> ImportMap l
mkImportMap options progs imports =
  if not (optsLenient options) then Map.fromList
    [ (progName, progEnv) | Program{..} <- progs, progPath `elem` imports ]
  else
    let (direct, indirect) = partition (\ p -> progPath p `elem` imports) progs
    in Map.fromList $
        (++) [ (progName, progEnv) | Program{..} <- indirect ]
             [ (progName, progEnv) | Program{..} <- direct ]

-- Recursively Typecheck all the Modules ---------------------------------------

type ModuleMap = Map.Map FilePath (ThriftFile SpliceFile Loc)

-- Topologically sort the modules so that we can typecheck them. We will start
-- at the leaves and work our way up to the original module
sortModules :: ModuleMap -> Either [TypeError l] [ThriftFile SpliceFile Loc]
sortModules moduleMap = traverse getVertex sccs
  where
    sccs  = stronglyConnComp graph
    graph = map mkVertex (Map.toList moduleMap)
    -- Create a vertex from a map entry
    mkVertex (filepath, file) = (file, filepath, getIncludes file)
    getIncludes ThriftFile{..} = foldr getInc [] thriftHeaders
    getInc HInclude{incType=Include,..} ps = incPath : ps
    getInc _ ps = ps
    -- get a ThriftFile from an SCC
    getVertex (AcyclicSCC f) = pure f
    getVertex (CyclicSCC fs) = Left [CyclicModules fs]

-- Prune the modules to only include the required symbols. This avoids
-- generating extra code
-- Note: assumes modules are topologically sorted with head module at the tail
pruneModules
  :: Typecheckable l
  => Options l
  -> [ThriftFile SpliceFile Loc]
  -> [ThriftFile SpliceFile Loc]
pruneModules Options { optsReqSymbols = Nothing } files = files
pruneModules opts@Options { optsReqSymbols = Just syms } files =
  prunesFiles
    where
      (prunesFiles, _, _) =
        foldr (pruneModule opts) ([], symbolMap, mempty) files
      symbolMap = mkSymbolMap opts syms

type SymbolMap = Map.Map Text Symbols
type Symbols   = Set.Set Text

mkSymbolMap :: Options l -> [Text] -> SymbolMap
mkSymbolMap Options{..} qsyms = Map.fromListWith Set.union
  [ (mname, Set.singleton symbol)
  | s <- qsyms
  , let
      (pre, symbol) = Text.breakOnEnd "." s
      mname | Text.null pre = topModule
            | otherwise = Text.dropEnd 1 pre
  ]
  where
    topModule = getModuleName optsPath

pruneModule
  :: Typecheckable l
  => Options l
  -> ThriftFile SpliceFile Loc
  -> ([ThriftFile SpliceFile Loc], SymbolMap, Interface l)
  -> ([ThriftFile SpliceFile Loc], SymbolMap, Interface l)
pruneModule opts file@ThriftFile{..} (fs, symbolMap, iface) =
  (prunedFile : fs, newMap, iface')
  where
    prunedFile = file { thriftDecls = prunedDecls }
    (prunedDecls, _, newMap) =
      filterDecls requiredSymbols symbolMap' thriftDecls
    requiredSymbols = fromMaybe Set.empty $ Map.lookup thriftName symbolMap'

    symbolMap' =
      Map.unionWith Set.union symbolMap $ mkSymbolMap opts extraSymbols
    extraSymbols = getExtraSymbols opts iface' file
    iface' = iface <> getInterface opts file

filterDecls
  :: Symbols
  -> SymbolMap
  -> [Parsed Decl]
  -> ([Parsed Decl], Symbols, SymbolMap)
filterDecls reqSymbols symbolMap =
  foldr filterSCC ([], reqSymbols, symbolMap) . mkSortedGraph
  where
    -- If we get a singleton node and it's a member of the required symbols,
    -- include it with all dependencies
    filterSCC (AcyclicSCC node) ctx@(_, symbols, _)
      | isRequired node symbols = addNode node ctx
    -- If we get a strongly connected component and at least one of the decls
    -- is required, we need to include all of them
    filterSCC (CyclicSCC nodes) ctx@(_, symbols, _)
      | any (\node -> isRequired node symbols) nodes =
        foldr addNode ctx nodes
    -- Otherwise we don't need it
    filterSCC _ ctx = ctx

    filterOutFns symbols (FunctionStmt Function{..}) = Set.member funName symbols
    filterOutFns _ (PerformsStmt _) = False

    isRequired (D_Service Service{..}, dname, _) symbols =
      Set.member dname symbols ||
      any (filterOutFns symbols) serviceStmts
    isRequired (_, dname, _) symbols = Set.member dname symbols

    addNode node (ds, syms, smap) = (decl : ds, newSyms, newMap)
      where
        (decl, _, rsyms) = filterDecl node syms
        (newSyms, newMap) = foldr addSym (syms, smap) rsyms

    -- If the Decl is a Service, then we only want to take the functions which
    -- are required
    filterDecl (D_Service s, _, _) symbols = mkVertex newDecl
      where
        newDecl = D_Service s {serviceStmts = filteredStmts, serviceSAnns=[]}

        mapFunSAnns (FunctionStmt f) = FunctionStmt (f {funSAnns=[]})
        mapFunSAnns p = p

        filteredStmts =
          map mapFunSAnns $
          filter (filterOutFns symbols) $
          serviceStmts s
    filterDecl (D_Interaction s, _, _) symbols = mkVertex newDecl
      where
        newDecl = D_Interaction s {interactionFunctions = filteredFuns, interactionSAnns=[]}
        filteredFuns =
          map (\f -> f {funSAnns=[]}) $
          filter (\Function{..} -> Set.member funName symbols) $
          interactionFunctions s
    filterDecl (D_Struct s, syms, smap) _ =
      (D_Struct s
        { structSAnns = []
        , structMembers = map filterField $ structMembers s
        }, syms, smap)
    filterDecl (D_Union u, syms, smap) _ =
      (D_Union u
        { unionSAnns = []
        , unionAlts = map filterAlt $ unionAlts u
        }, syms, smap)
    filterDecl (D_Typedef t, syms, smap) _ =
      (D_Typedef t {tdSAnns=[]}, syms, smap)
    filterDecl (D_Enum e, syms, smap) _ =
        (D_Enum e
          {enumSAnns=[]
          , enumConstants = filterEnumConstants $ enumConstants e
          }, syms, smap)
      where
        filterEnumConstants = map filterEnumValue
        filterEnumValue ev = ev { evSAnns = []}
    filterDecl (D_Const c, syms, smap) _ =
      (D_Const c {constSAnns=[]}, syms, smap)
    filterField field = field {fieldSAnns=[]}
    filterAlt alt = alt {altSAnns=[]}
    addSym symbol (syms, smap)
      | Text.null name = (Set.insert symbol syms, smap)
      | otherwise = (syms, Map.insertWith Set.union prefix nameSet smap)
      where (prefix, name) = Text.breakOn "." symbol
            nameSet = Set.singleton $ Text.drop 1 name

    mkSortedGraph :: [Parsed Decl] -> [SCC (Parsed Decl, Text, [Text])]
    mkSortedGraph = stronglyConnCompR . map mkVertex

    mkVertex d@(D_Struct Struct{..}) =
      (d, structName, concatMap fieldSymbols structMembers)
    mkVertex d@(D_Union Union{..}) =
      (d, unionName, concatMap altSymbols unionAlts)
    mkVertex d@(D_Enum Enum{..}) =
      (d, enumName, [])
    mkVertex d@(D_Typedef Typedef{..}) =
      (d, tdName, anTypeSymbols tdType)
    mkVertex d@(D_Const Const{..}) =
      (d, constName, anTypeSymbols constType ++ constSymbols constVal)
    mkVertex d@(D_Service Service{..}) =
      ( d
      , serviceName
      , maybeToList (supName <$> serviceSuper) ++
        concatMap stmtSymbols serviceStmts
      )
    mkVertex d@(D_Interaction Interaction{..}) =
      ( d
      , interactionName
      , maybeToList (supName <$> interactionSuper) ++
        concatMap funSymbols interactionFunctions
      )
    fieldSymbols :: Parsed (Field u) -> [Text]
    fieldSymbols Field{..} =
      maybe [] constSymbols fieldVal ++ anTypeSymbols fieldType
    altSymbols :: Parsed UnionAlt -> [Text]
    altSymbols UnionAlt{..} = anTypeSymbols altType
    constSymbols (UntypedConst _ val) = constValSymbols val
    constValSymbols IntConst{} = []
    constValSymbols DoubleConst{} = []
    constValSymbols StringConst{} = []
    constValSymbols BoolConst{} = []
    constValSymbols (IdConst name) = [name]
    constValSymbols ListConst{..} = concatMap (constSymbols . leElem) lvElems
    constValSymbols MapConst{..} =
      [ s | ListElem{leElem=MapPair{..}} <- mvElems
          , s <- constSymbols mpKey ++ constSymbols mpVal ]
    constValSymbols StructConst{..} =
      [ s | ListElem{leElem=StructPair{..}} <- svElems
          , s <- constSymbols spVal ]
    stmtSymbols :: Parsed ServiceStmt -> [Text]
    stmtSymbols (FunctionStmt f) = funSymbols f
    stmtSymbols _ = []
    funSymbols :: Parsed Function -> [Text]
    funSymbols Function{..} =
      funName :
      funTypeSymbols funType ++
      concatMap fieldSymbols funArgs ++
      concatMap fieldSymbols funExceptions
    funTypeSymbols :: Parsed FunctionType -> [Text]
    funTypeSymbols (FunType (Some ty)) = anTypeSymbols ty
    funTypeSymbols (FunTypeVoid _) = []
    funTypeSymbols
      (FunTypeResponseAndStreamReturn
       ResponseAndStreamReturn{rsStream=Stream{..}}) =
        anTypeSymbols streamType ++
        maybe [] (concatMap fieldSymbols . throwsFields) streamThrows

    anTypeSymbols :: AnnotatedType Loc t -> [Text]
    anTypeSymbols AnnotatedType{..} = typeSymbols atType
    typeSymbols :: TType 'Unresolved l Loc t -> [Text]
    -- Base types have no dependencies
    typeSymbols I8  = []
    typeSymbols I16 = []
    typeSymbols I32 = []
    typeSymbols I64 = []
    typeSymbols TFloat  = []
    typeSymbols TDouble = []
    typeSymbols TBool = []
    typeSymbols TText = []
    typeSymbols TBytes = []
    -- Recursive types have recursive dependencies
    typeSymbols (TSet ty)      = anTypeSymbols ty
    typeSymbols (THashSet ty)  = anTypeSymbols ty
    typeSymbols (TList ty)     = anTypeSymbols ty
    typeSymbols (TMap k v)     = anTypeSymbols k ++ anTypeSymbols v
    typeSymbols (THashMap k v) = anTypeSymbols k ++ anTypeSymbols v
    -- Named types depend on the type they name
    typeSymbols (TNamed name) = [name]

-- Resolve the Decls -----------------------------------------------------------

resolveDecl :: Typecheckable l => Parsed Decl -> Typechecked l Decl
resolveDecl (D_Struct s)  = D_Struct  <$> resolveStruct s
resolveDecl (D_Union u)   = D_Union   <$> resolveUnion u
resolveDecl (D_Typedef t) = D_Typedef <$> resolveTypedef t
resolveDecl (D_Enum e)    = D_Enum    <$> resolveEnum e
resolveDecl (D_Const c)   = D_Const   <$> resolveConst c
resolveDecl (D_Service s) = D_Service <$> resolveService s
resolveDecl (D_Interaction s) = D_Interaction <$> resolveInteraction s

resolveTypedef
  :: forall l. Typecheckable l
  => Parsed Typedef
  -> Typechecked l Typedef
resolveTypedef t@Typedef{..} = mkTypedef
  where
    mkTypedef :: Typechecked l Typedef
    mkTypedef = do
      Env{..} <- ask
      thisty <- resolveAnnotatedType tdType
      sAnns   <- resolveStructuredAnns tdSAnns
      declIsNewtype <- or <$> mapM checkAnn (filterHsAnns $ getAnns tdAnns)
      case thisty of
        Some ty -> return $ Typedef
          { tdResolvedName = renameTypedef options t
          , tdResolvedType = ty
          , tdTag = if declIsNewtype then IsNewtype else IsTypedef
          , tdSAnns = sAnns
          , ..
          }

    checkAnn SimpleAnn{..} | saTag == "hs.newtype" = return True
    checkAnn ann = typeError (annLoc ann) $ AnnotationMismatch AnnTypedef ann

resolveStruct
  :: Typecheckable l
  => Parsed Struct
  -> Typechecked l Struct
resolveStruct s@Struct{..} = do
  Env{..} <- ask
  fields <- resolveFields structName (getAnns structAnns) structMembers
  sAnns   <- resolveStructuredAnns structSAnns
  return Struct
    { structResolvedName = renameStruct options s
    , structMembers      = fields
    , structSAnns        = sAnns
    , ..
    }

resolveFields
  :: Typecheckable l
  => Text
  -> [Annotation Loc]
  -> [Parsed (Field u)]
  -> TC l [Field u 'Resolved l Loc]
resolveFields sname as fs =
  (traverse (resolveField as sname) =<< fields) <*
  -- Check for duplicate field ids
  foldM checkId Set.empty fs
  where
    update lazy f = f { fieldLaziness = lazy }
    fields = foldM checkAnn fs $ filterHsAnns as
    checkAnn _ SimpleAnn{..}
      | saTag == "hs.strict" = pure $ map (update Strict) fs
      | saTag == "hs.lazy"   = pure $ map (update Lazy) fs
    checkAnn fs' ValueAnn{..}
      | vaTag == "hs.prefix"
      , TextAnn{} <- vaVal = pure fs'
    checkAnn _ ann = typeError (annLoc ann) $ AnnotationMismatch AnnStruct ann
    checkId ids Field{..}
      | Set.member fieldId ids =
          typeError (lLocation $ flId fieldLoc) $
          InvalidFieldId fieldName (fromIntegral fieldId)
      | otherwise = pure $ Set.insert fieldId ids

getPriority
  :: [Annotation Loc]
  -> Maybe ThriftPriority
getPriority = listToMaybe . mapMaybe getP
  where
    getP ValueAnn{vaVal=TextAnn p _, vaTag = "priority"}
      | p == "HIGH_IMPORTANT" = Just HighImportant
      | p == "HIGH"           = Just High
      | p == "IMPORTANT"      = Just Important
      | p == "NORMAL"         = Just NormalPriority
      | p == "BEST_EFFORT"    = Just BestEffort
      | p == "N_PRIORITIES"   = Just NPriorities
    getP _ = Nothing

resolveField
  :: Typecheckable l
  => [Annotation Loc]
  -> Text
  -> Parsed (Field u)
  -> Typechecked l (Field u)
resolveField anns sname field@Field{..} = do
  when (fieldId == 0) $
    typeError (lLocation $ flId fieldLoc) $ InvalidFieldId fieldName 0
  thisty <- resolveAnnotatedType fieldType
  case thisty of
    Some ty -> do
      val  <- sequence (typecheckConst ty <$> fieldVal)
      lazy <- case filterHsAnns $ getAnns fieldAnns of
            [SimpleAnn{..}]
              | saTag == "hs.strict" -> pure Strict
              | saTag == "hs.lazy"   -> pure Lazy
            [] -> pure fieldLaziness
            ann : _ -> typeError (annLoc ann) $ AnnotationMismatch AnnField ann
      sAnns <- resolveStructuredAnns fieldSAnns
      Env{..} <- ask
      case resolveTag fieldTag ty of
        Nothing -> typeError (lLocation $ flName fieldLoc) $
                   InvalidThrows ty fieldName
        Just tag ->
          return Field
          { fieldResolvedName = renameField options anns sname field
          , fieldResolvedType = ty
          , fieldResolvedVal  = val
          , fieldLaziness = lazy
          , fieldTag = tag
          , fieldSAnns = sAnns
          , ..
          }

resolveTag
  :: FieldTag u 'Unresolved () a
  -> Type l t
  -> Maybe (FieldTag u 'Resolved l t)
resolveTag STRUCT_FIELD _ = Just STRUCT_FIELD
resolveTag ARGUMENT     _ = Just ARGUMENT
resolveTag THROWS_UNRESOLVED ty =
  case getAliasedType ty of
    TException{} -> Just THROWS_RESOLVED
    _ -> Nothing

resolveUnion :: forall l. Typecheckable l => Parsed Union -> Typechecked l Union
resolveUnion u@Union{..} = do
  Options{optsLenient} <- asks options
  alts <- resolveAlts optsLenient unionAlts
  thishasEmpty <-
    fromMaybe (Some HasEmpty) . listToMaybe . catMaybes <$>
    mapM resolveAnn (filterHsAnns $ getAnns unionAnns)
  sAnns <- resolveStructuredAnns unionSAnns

  Env{..} <- ask
  case thishasEmpty of
    Some hasEmpty -> return Union
      { unionResolvedName = renameUnion options u
      , unionAlts = alts
      , unionEmptyName = getEmptyName options hasEmpty
      , unionHasEmpty  = hasEmpty
      , unionSAnns = sAnns
      , ..
      }
  where
    resolveAlts :: Bool -> [Parsed UnionAlt] -> TC l [UnionAlt 'Resolved l Loc]
    resolveAlts optsLenient alts =
      traverse resolveAlt alts
      -- Check for duplicate field ids
      <* foldM checkId Set.empty alts
      <* checkEmpty
      where
        checkId ids UnionAlt{..}
          | Set.member altId ids =
              typeError (lLocation $ flId altLoc) $
              InvalidFieldId altName (fromIntegral altId)
          | otherwise = pure $ Set.insert altId ids
        checkEmpty = when (null alts && not optsLenient) $
          typeError (lLocation $ slKeyword unionLoc) $ EmptyUnion unionName

    resolveAlt :: Parsed UnionAlt -> Typechecked l UnionAlt
    resolveAlt alt@UnionAlt{..} = do
      thisty <- resolveAnnotatedType altType
      sAnns   <- resolveStructuredAnns altSAnns
      Env{..} <- ask
      case thisty of
        Some ty -> return UnionAlt
          { altResolvedName = renameUnionAlt options u alt
          , altResolvedType = ty
          , altSAnns = sAnns
          , ..
          }

    resolveAnn :: Annotation Loc -> TC l (Maybe (Some PossiblyEmpty))
    resolveAnn SimpleAnn{..}
      | saTag == "hs.nonempty" = pure $ Just $ Some NonEmpty
    resolveAnn ValueAnn{..}
      | vaTag == "hs.prefix"
      , TextAnn{} <- vaVal = pure Nothing
    resolveAnn a = typeError (annLoc a) $ AnnotationMismatch AnnUnion a

    getEmptyName :: Options l -> PossiblyEmpty e -> EmptyName 'Resolved e
    getEmptyName opts HasEmpty = getUnionEmptyName opts u
    getEmptyName _ NonEmpty = ()

resolveEnum :: forall l. Typecheckable l => Parsed Enum -> Typechecked l Enum
resolveEnum enum@Enum{..} = do
  consts <- mapM mkAlt enumConstants
  sAnns   <- resolveStructuredAnns enumSAnns
  Env{..} <- ask
  forM_ getDups $
    typeError $ lLocation $ slName enumLoc
  return Enum
    { enumResolvedName = renameEnum options enum
    , enumConstants = consts
    , enumFlavour = enumFlavourTag options enum
    , enumSAnns = sAnns
    , ..
    }
  where
    mkAlt :: Parsed EnumValue -> Typechecked l EnumValue
    mkAlt EnumValue{..} = do
      sAnns   <- resolveStructuredAnns evSAnns
      Env{..} <- ask
      return EnumValue
        { evResolvedName = renameEnumAlt options enum evName
        , evSAnns = sAnns
        , ..
        }

    getDups :: Maybe (ErrorMsg l)
    getDups =
      (\(v, ns) -> DuplicateEnumVal enumName ns v) <$>
      find ((>1) . length . snd)
      (Map.toList $
       Map.fromListWith (++)
       [ (fromIntegral evValue, [evName]) | EnumValue{..} <- enumConstants ])

resolveConst :: Typecheckable l => Parsed Const -> Typechecked l Const
resolveConst Const{..} = do
  thisty <- resolveAnnotatedType constType
  case thisty of
    Some ty -> do
      Env{..} <- ask
      val   <- typecheckConst ty constVal
      sAnns <- resolveStructuredAnns constSAnns
      return Const
        { constResolvedName = renameConst options constName
        , constResolvedType = ty
        , constResolvedVal  = val
        , constSAnns        = sAnns
        , ..
        }

resolveService :: Typecheckable l => Parsed Service -> Typechecked l Service
resolveService s@Service{..} = do
  (super, stmts, sAnns)
    <- (,,) <$> sequence (resolveSuper <$> serviceSuper)
            <*> traverse resolveStmt serviceStmts
            <*> resolveStructuredAnns serviceSAnns
  Env{..} <- ask
  pure Service
    { serviceResolvedName = renameService options s
    , serviceSuper        = super
    , serviceStmts        = stmts
    , serviceSAnns        = sAnns
    , ..
    }
  where
    resolveSuper Super{..} = do
      name <- mkThriftName supName
      (rname, _, rloc) <- lookupService name $ lLocation $ slName serviceLoc
      pure Super { supResolvedName = (rname, rloc), .. }

resolveStmt :: Typecheckable l => Parsed ServiceStmt -> Typechecked l ServiceStmt
resolveStmt (FunctionStmt f) = do
  r <- resolveFunction f
  pure $ FunctionStmt r
resolveStmt (PerformsStmt Performs{..}) = do
  pure $ PerformsStmt $ Performs {..}

resolveInteraction :: Typecheckable l => Parsed Interaction -> Typechecked l Interaction
resolveInteraction Interaction{..} = do
  (super, funs, sAnns)
    <- (,,) <$> sequence (resolveSuper <$> interactionSuper)
            <*> traverse resolveFunction interactionFunctions
            <*> resolveStructuredAnns interactionSAnns
  pure Interaction
    { interactionResolvedName = interactionName
    , interactionSuper        = super
    , interactionFunctions    = funs
    , interactionSAnns        = sAnns
    , ..
    }
  where
    resolveSuper Super{..} = do
      name <- mkThriftName supName
      (rname, _, rloc) <- lookupService name $ lLocation $ slName interactionLoc
      pure Super { supResolvedName = (rname, rloc), .. }

resolveFunction :: Typecheckable l => Parsed Function -> Typechecked l Function
resolveFunction f@Function{..} = do
  (rtype, ftype, args, excepts, sAnns)
    <- (,,,,) <$>
        sequence (resolveFunctionTypeTy funType)
          <*> resolveFunctionType funName annNoPriorities funType
          <*> resolveFields funName annNoPriorities funArgs
          <*> resolveFields funName annNoPriorities funExceptions
          <*> resolveStructuredAnns funSAnns
  Env{..} <- ask
  pure $ Function
    { funResolvedName = renameFunction options f
    , funType         = ftype
    , funResolvedType = rtype
    , funArgs         = args
    , funExceptions   = excepts
    , funPriority = fromMaybe funPriority $ getPriority annPriorities
    , funSAnns = sAnns
    , ..
    }
  where
    (annPriorities, annNoPriorities) = partition isPriority $ getAnns funAnns
    isPriority ValueAnn{..}
      | TextAnn{} <- vaVal = vaTag == "priority"
    isPriority _ = False

resolveFunctionTypeTy
  :: Typecheckable l
  => FunctionType s () Loc
  -> Maybe (TC l (Some (Type l)))
resolveFunctionTypeTy (FunType (Some ty)) =
  Just $ resolveAnnotatedType ty
resolveFunctionTypeTy (FunTypeVoid _) =
  Nothing
resolveFunctionTypeTy (FunTypeResponseAndStreamReturn _) =
  Nothing -- This doesn't support stream yet

resolveFunctionType
  :: Typecheckable l
  => Text
  -> [Annotation Loc]
  -> Parsed FunctionType
  -> Typechecked l FunctionType
resolveFunctionType _ _ (FunType (Some ty)) = pure $ FunType (Some ty)
resolveFunctionType _ _ (FunTypeVoid ann) = pure $ FunTypeVoid ann
resolveFunctionType
  structName
  ann
  (FunTypeResponseAndStreamReturn ResponseAndStreamReturn{..}) = do
    stream <- resolveStream rsStream
    pure $ FunTypeResponseAndStreamReturn $ ResponseAndStreamReturn
      { rsStream = stream
      , ..
      }
    where
      resolveStream Stream{..} = do
        throws <- mapM resolveThrows streamThrows
        pure $ Stream
          { streamThrows = throws
          , ..
          }
      resolveThrows Throws{..} = do
        fields <- resolveFields structName ann throwsFields
        pure $ Throws
          { throwsFields = fields
          , ..
          }

-- Resolve Structured Annotations ----------------------------------------------

resolveStructuredAnns
  :: Typecheckable l
  => [StructuredAnnotation 'Unresolved () Loc]
  -> TC l [StructuredAnnotation 'Resolved  l Loc]
resolveStructuredAnns = mapM resolveStructuredAnn

resolveStructuredAnn
  :: Typecheckable l
  => Parsed StructuredAnnotation
  -> Typechecked l StructuredAnnotation
resolveStructuredAnn StructuredAnn{..} = do
  thisty <- resolveType (TNamed saType) saTypeLoc Nothing
  saTypeName <- mkThriftName saType
  thisschema <- lookupSchemaRec saTypeName lLocation
  case (thisty, thisschema) of
    (Some ty, Some schema) -> do
      val <- typecheckStruct lLocation schema =<<
             mkStructFieldMap (maybe [] saElems saMaybeElems)
      case ty of
        TStruct _ loc | not $ loc `isDefinedAt` lLocation ->
            typeError lLocation (NotDefinedBeforeUsed ty)
        _ ->
            pure StructuredAnn
            { saResolvedType = ty
            , saResolvedVal = Some val
            , ..
            }
  where
    Located{..} = a0Ty saTypeLoc
    isDefinedAt :: Loc -> Loc -> Bool
    isDefinedAt defLoc loc
      | locFile defLoc /= locFile loc = True
      | locEndLine defLoc < locStartLine loc = True
      | locEndLine defLoc > locStartLine loc = False
      -- locEndLine defLoc == locStartLine loc
      | locEndCol defLoc <= locStartCol loc = True
      | otherwise = False
    lookupSchemaRec :: ThriftName -> Loc -> TC l (Some (Schema l))
    lookupSchemaRec name loc = do
      thisty <- lookupType name loc
      case thisty of
        Some ty -> do
          let nm = case getAliasedType ty of
                TStruct Name{..} _ -> sourceName
                _ -> name
          lookupSchema nm loc

-- Build the Constant Map ------------------------------------------------------

mkConstMap
  :: forall l. Typecheckable l
  => (Text, Options l)
  -> ImportMap l
  -> TypeMap l
  -> [Parsed Decl]
  -> Either [TypeError l] (ConstMap l)
mkConstMap (thriftName, opts) imap tmap = foldM insertC emptyContext
  where
    -- Structs don't define constants, but they have symbols in scope
    insertC m (D_Struct s@Struct{..}) = do
      -- add data constructor name to scope
      scope <- addToScope (lLocation $ slName structLoc) (renameStruct opts s) m
      -- add field names to scope unless Duplicate Names is turned on
      if fieldsAreUnique opts
        then foldM (insFieldName opts) scope structMembers
        else return scope
      where
        insFieldName
          :: Options l
          -> ConstMap l
          -> Parsed (Field u)
          -> Either [TypeError l] (ConstMap l)
        insFieldName opts' scope field@Field{..} =
          addToScope (lLocation $ flName fieldLoc)
          (renameField opts' (getAnns structAnns) structName field)
          scope

    -- Unions define data constructors
    insertC m (D_Union u@Union{..})
      | unionAltsAreUnique opts =
          (if any isETag $ getAnns unionAnns
           then pure
           else addToScope (lLocation $ slName unionLoc)
                (getUnionEmptyName opts u))
          =<< foldM (addAlt opts) m unionAlts
      | otherwise = pure m
      where
        addAlt
          :: Options l
          -> ConstMap l
          -> Parsed UnionAlt
          -> Either [TypeError l] (ConstMap l)
        addAlt opts' scope alt@UnionAlt{..} =
          addToScope (lLocation $ flName altLoc) (renameUnionAlt opts' u alt)
            scope
        isETag SimpleAnn{..} = saTag == "hs.nonempty"
        isETag ValueAnn{} = False

    -- If a typedef is s newtype, we have to add the data constructor to the
    -- scope
    insertC m (D_Typedef t@Typedef{..})
      | isNewtype (getAnns tdAnns) =
          let
            name = renameTypedef opts t
            loc  = lLocation $ tdlName tdLoc
          in addToScope loc name =<< addToScope loc ("un" <> name) m
      | otherwise = pure m

    -- All the enum constants have the type of the enum
    insertC m (D_Enum enum@Enum{..})
      | enumAltsAreUnique opts = foldM insE m enumConstants
      | otherwise = pure m
      where
        insE acc EnumValue{..} = addToScope loc renamed acc
          where
            renamed = renameEnumAlt opts enum evName
            loc = lLocation $ evlName evLoc
    insertC m (D_Const Const{..}) = do
      ty <- runTypechecker env $ resolveAnnotatedType constType
      let renamed = renameConst opts constName
      insertContext loc constName renamed (ty, mkName constName renamed, loc) m
      where
        env = (emptyEnv (thriftName, opts))
          { typeMap   = tmap
          , constMap  = m
          , importMap = imap
          }
        loc = lLocation $ clName constLoc
    insertC m D_Service{} = pure m
    insertC m D_Interaction{} = pure m

getEnumType :: Typecheckable l => Options l -> Parsed Enum -> Some (Type l)
getEnumType opts enum@Enum{..} = case enumFlavourTag opts enum of
  PseudoEnum{} -> Some $ TNewtype name enumValueType loc
  SumTypeEnum noUnknown -> Some $ TEnum name loc noUnknown
  where
    name = mkName enumName $ renameEnum opts enum
    loc = lLocation (slName enumLoc)

-- Typecheck Constants ---------------------------------------------------------

typecheckConst
   :: Typecheckable l
   => Type l t
   -> UntypedConst Loc
   -> TC l (TypedConst l t)

-- Integral Types
-- Maybe we should do overflow checking here?
typecheckConst I8  (UntypedConst _ (IntConst i _)) = literal $ fromIntegral i
typecheckConst I16 (UntypedConst _ (IntConst i _)) = literal $ fromIntegral i
typecheckConst I32 (UntypedConst _ (IntConst i _)) = literal $ fromIntegral i
typecheckConst I64 (UntypedConst _ (IntConst i _)) = literal $ fromIntegral i

-- Floating Point Types
typecheckConst TDouble (UntypedConst _ (IntConst i _)) =
  literal $ fromIntegral i
typecheckConst TDouble (UntypedConst _ (DoubleConst d _)) = literal d
typecheckConst TFloat (UntypedConst _ (IntConst i _)) = literal $ fromIntegral i
typecheckConst TFloat (UntypedConst _ (DoubleConst d _)) =
  literal $ double2Float d

-- Other Base Types
typecheckConst TBool (UntypedConst _ (IntConst i _))
  | i == 0 = literal False
  | i == 1 = literal True
typecheckConst TBool (UntypedConst _ (BoolConst b)) = literal b
typecheckConst TText (UntypedConst _ (StringConst s _)) = literal s
typecheckConst TBytes (UntypedConst _ (StringConst s _)) =
  literal $ encodeUtf8 s

-- Recursive Types
typecheckConst (TList u) (UntypedConst _ ListConst{..}) =
  Literal . List <$> traverse (typecheckConst u . leElem) lvElems
typecheckConst ty@(TList _) c@(UntypedConst l MapConst{mvElems=[]})  = do
  Options{optsLenient} <- asks options
  if optsLenient then
    return $ Literal $ List [] -- weird files use the wrong empty brackets, sigh
  else
    typeError (lLocation l) (LiteralMismatch ty c)
typecheckConst (TSet u) (UntypedConst _ ListConst{..}) =
  Literal . Set <$> traverse (typecheckConst u . leElem) lvElems
typecheckConst ty@(TSet _) c@(UntypedConst l MapConst{mvElems=[]}) = do
  Options{optsLenient} <- asks options
  if optsLenient then
    return $ Literal $ Set [] -- weird files use the wrong empty brackets, sigh
  else
    typeError (lLocation l) (LiteralMismatch ty c)
typecheckConst (THashSet u) (UntypedConst _ ListConst{..}) =
  Literal . HashSet <$> traverse (typecheckConst u . leElem) lvElems
typecheckConst ty@(THashSet _) c@(UntypedConst l MapConst{mvElems=[]}) = do
  Options{optsLenient} <- asks options
  if optsLenient then
    return $ Literal $ HashSet [] -- weird files use the wrong empty brackets
  else
    typeError (lLocation l) (LiteralMismatch ty c)
typecheckConst (TMap kt vt) (UntypedConst _ MapConst{..}) = do
  Options{optsLenient} <- asks options
  Literal . Map <$> traverseWeird optsLenient tcConsts mvElems
    where
      tcConsts ListElem{leElem=MapPair{..}} = (,)
        <$> typecheckConst kt mpKey
        <*> typecheckConst vt mpVal
typecheckConst ty@(TMap _ _) c@(UntypedConst l ListConst{lvElems=[]}) = do
  Options{optsLenient} <- asks options
  if optsLenient then
    return $ Literal $ Map [] -- weird files use the wrong empty brackets, sigh
  else
    typeError (lLocation l) (LiteralMismatch ty c)
typecheckConst (THashMap kt vt) (UntypedConst _ MapConst{..}) =
  Literal . HashMap <$> traverse tcConsts mvElems
    where
      tcConsts ListElem{leElem=MapPair{..}} = (,)
        <$> typecheckConst kt mpKey
        <*> typecheckConst vt mpVal
typecheckConst ty@(THashMap _ _) c@(UntypedConst l ListConst{lvElems=[]}) = do
  Options{optsLenient} <- asks options
  if optsLenient then
    return $ Literal $ HashMap [] -- weird files use the wrong empty brackets
  else
    typeError (lLocation l) (LiteralMismatch ty c)

-- Enums
typecheckConst e@(TEnum Name{..} _loc _)
               lit@(UntypedConst Located{..} (IntConst i _)) = do
  (emap, _) <- lookupEnum sourceName lLocation
  case Map.lookup (fromIntegral i) emap of
    Nothing -> typeError lLocation $ LiteralMismatch e lit
    Just (constName, constLoc) -> literal $ EnumVal constName constLoc
typecheckConst enum@(TEnum name _loc _)
               (UntypedConst Located{..} (IdConst ident)) =
  typecheckEnum enum lLocation name ident <|>
    typecheckIdent enum lLocation ident

-- Typedefs
typecheckConst (TTypedef _ ty _loc) val = typecheckConst ty val

-- Newtypes
-- This case is a bit complicated
typecheckConst newt@(TNewtype name ty _loc) val@(UntypedConst Located{..} c) =
   case c of
     -- If it's an identifier, then it needs to be an exact type match,
     -- however IdConsts can also be enums, so we have to check for this case
     -- too
     IdConst ident ->
       typecheckPseudoEnum newt lLocation name ident <|>
       typecheckIdent newt lLocation ident <|>
       (liftNew =<< typecheckConst ty val)
     _ -> liftNew =<< typecheckConst ty val
  where
    -- Lift literals into the newtype, but reject identifiers
    liftNew (Literal lit) = literal $ New lit
    liftNew (Identifier Name{..} u _loc) =
      typeError lLocation $ IdentMismatch newt u sourceName
    liftNew (WeirdEnumToInt u Name{..} _tyEnum _loc) =
      typeError lLocation $ IdentMismatch newt u sourceName

-- Structs
typecheckConst (TStruct Name{..} _loc)
               (UntypedConst Located{..} MapConst{..}) = do
  tschema <- lookupSchema sourceName lLocation
  case tschema of
    Some schema -> Literal . Some
               <$> (typecheckStruct lLocation schema =<< mkFieldMap mvElems)
typecheckConst tyTop@(TStruct Name{} _loc)
               (UntypedConst Located{..} StructConst{..}) = do
  -- First typecheck the struct with the type annotated then check this matches
  -- the type given at the top level
  svTypeName <- mkThriftName svType
  tschema <- lookupSchema svTypeName lLocation
  ttyAnn <- lookupType svTypeName lLocation
  case (tschema, ttyAnn) of
    (Some schema, Some tyAnn) -> do
      let struct = typecheckStruct lLocation schema =<< mkStructFieldMap svElems
      case eqOrAlias tyTop tyAnn of
        Just _ -> Literal . Some <$> struct
        Nothing -> typeError lLocation $ TypeMismatch tyTop tyAnn
typecheckConst (TException Name{..} _loc)
               (UntypedConst Located{..} MapConst{..}) = do
  tschema <- lookupSchema sourceName lLocation
  case tschema of
    Some schema -> Literal . Some . EV <$>
                   (typecheckStruct lLocation schema =<< mkFieldMap mvElems)
typecheckConst (TUnion n@Name{..} _loc) (UntypedConst uloc MapConst{..}) = do
  thisschema <- lookupUnion sourceName (lLocation uloc)
  -- Unions can only have one field
  (utname, val) <-
    case mvElems of
      [ListElem{leElem=MapPair{..}}] -> pure (mpKey, mpVal)
      l -> typeError (lLocation uloc) $ InvalidUnion n (length l)
  fname <-
    case utname of
      UntypedConst _ (StringConst s _) -> pure s
      v@(UntypedConst utloc _) -> typeError (lLocation utloc) $ InvalidField v
  case thisschema of
    Some schema -> Literal . Some <$> typecheckUnion schema fname val
typecheckConst
  tyTop@(TUnion n@Name{} _loc)
  (UntypedConst Located{..} StructConst {..}) = do
    -- Constant structs can be used for unions as well
    svTypeName <- mkThriftName svType
    tschema <- lookupUnion svTypeName lLocation
    ttyAnn <- lookupType svTypeName lLocation
    case (tschema, ttyAnn) of
      (Some schema, Some tyAnn) -> case svElems of
        [ListElem{leElem = StructPair{..}}] -> do
          case eqOrAlias tyTop tyAnn of
            Just _ -> Literal . Some <$> typecheckUnion schema spKey spVal
            Nothing -> typeError lLocation $ TypeMismatch tyTop tyAnn
        _ -> typeError lLocation $ InvalidUnion n $ length svElems

-- Identifiers (typecheckIdentNum is not first parameter to <|>)
-- These identified are permitted to be enums in lenient mode
typecheckConst ty (UntypedConst Located{..} (IdConst ident)) = do
  Env{..} <- ask
  typecheckIdent ty lLocation ident
    <|> typecheckIdentNum ty lLocation ident
    <|> case ty of
      I8 | optsLenient options -> typecheckEnumAsInt lLocation ident
      I16 | optsLenient options -> typecheckEnumAsInt lLocation ident
      I32 | optsLenient options -> typecheckEnumAsInt lLocation ident
      I64 | optsLenient options -> typecheckEnumAsInt lLocation ident
      _ -> empty

-- Special Types
typecheckConst (TSpecial ty) val = typecheckSpecialConst ty val

-- Type Error
typecheckConst ty val@(UntypedConst Located{..} _) =
  typeError lLocation $ LiteralMismatch ty val

-- Lenient mode only. Implicit cast from a qualified enum to an int
typecheckEnumAsInt
  :: Integral t
  => Loc
  -> Text
  -> TC l (TypedConst l t)
typecheckEnumAsInt loc ident = do
  name <- mkThriftName ident
  let
    enumValue = case name of
      UName e -> UName <$> extractEnumValue e
      QName q e -> QName q <$> extractEnumValue e
    extractEnumValue text = if Text.null v
      then Nothing
      else Just $ Text.drop 1 v
      where (_nm, v) = Text.breakOn "." text
  case enumValue of
    Nothing -> empty
    Just k -> do
      i <- lookupEnumInt k loc
      literal $ fromIntegral i

-- NOTE: This function only handles identifiers that end in an enum value,
-- not identifiers that are other constants. Those are handled in a
-- separate typecheckIdent call in typecheckConst.
-- Also, enumTy and enumTyName need to match.
typecheckEnum
  :: Typecheckable l
  => Type l t
  -> Loc
  -> Name
  -> Text
  -> TC l (TypedConst l EnumVal)
typecheckEnum enumTy loc enumTyName ident = do
  -- Parse the identifier and look up its type.
  (identTyName, identValue) <- parseIdentifier
  identEnumType <- lookupType identTyName loc
  -- Check whether this is the same type as
  -- the declaration.
  -- Explicit () needed to avoid type error about GADTs and "untouchable"
  -- types.
  () <- case identEnumType of
    Some identEnumType' ->
      case enumTy `eqOrAlias` identEnumType' of
          Just Refl -> return ()
          Nothing -> typeError loc $ TypeMismatch enumTy identEnumType'
  -- Look up the enum values and see if `identValue` is one of them
  (_, nameMap) <- lookupEnum (sourceName enumTyName) loc
  case Map.lookup identValue nameMap of
    Nothing -> typeError loc $ UnknownEnumValue (sourceName enumTyName)
    Just (targetName, targetLoc) ->
      return $ Literal $ EnumVal targetName targetLoc
  where
    parseIdentifier = do
      -- This strips off the filename, if present, leaving us with either
      -- <enum>.<value> or just <value>. The latter case is technically
      -- out of spec, but it's out there, so we deal with it by substituting
      -- the expected enum typename in.
      tName <- mkThriftName ident
      -- We use the OnEnd methods so that if there is no dot, the content
      -- ends up in the suffix.
      let (pre, value) = Text.breakOnEnd "." (localName tName)
          enumName = if Text.null pre
            then localName $ sourceName enumTyName
            else Text.dropEnd 1 pre
          tyName = mapName (const enumName) tName
      return (tyName, value)


typecheckPseudoEnum
  :: (Typecheckable l)
  => Type l t
  -> Loc
  -> Name
  -> Text
  -> TC l (TypedConst l t)
typecheckPseudoEnum ty loc name@Name{..} ident = do
  result <- typecheckEnum ty loc name ident
  case result of
    Literal (EnumVal renamed locDefined) -> do
      thistrueTy <- lookupType sourceName loc
      case thistrueTy of
        Some trueTy -> case trueTy `eqOrAlias` ty of
          Just Refl -> pure $ Identifier renamed ty locDefined
          Nothing -> empty
    _ -> empty

-- | Handle weird case of enum to int casting, dispatch when 'ty' is int like.
--
-- Do not use as first parameter to '<|>'
typecheckIdentNum
   :: (Typecheckable l)
   => Type l t
   -> Loc
   -> Text
   -> TC l (TypedConst l t)
typecheckIdentNum ty loc ident = case ty of
  I8  -> typecheckEnumInt loc ident
  I16 -> typecheckEnumInt loc ident
  I32 -> typecheckEnumInt loc ident
  I64 -> typecheckEnumInt loc ident
  _ -> empty

-- | This is used for int-like constants that might, werdly, get their value
-- from an enum value (implicit casting is somewhay popular).  Use the
-- usual typecheckIdent before attempting the enum lookup (made up rule that
-- seems to work).
--
-- Do not use as first parameter to '<|>'
typecheckEnumInt
   :: (Typecheckable l, Num t)
   => Loc
   -> Text
   -> TC l (TypedConst l t)
typecheckEnumInt loc ident = do
  Env{..} <- ask
  if not (optsLenient options) then empty else do
    name <- mkThriftName ident
    i <- lookupEnumInt name loc
    literal $ fromIntegral i

-- | There a weird casting from enum to int that happens in libadmarket.thrift
--
-- > typedef i16 TimeZoneId
-- > const time_zone.TimeZoneType kReachBlockTimeZoneEST =
-- >    time_zone.TZ_AMERICA_NEW_YORK
-- > const TimeZoneId kDefaultReachBlockTimeZoneId = kReachBlockTimeZoneEST
--
-- To support this in the parser check 'useEnumAsInt'
typecheckIdent
  :: Typecheckable l
  => Type l t
  -> Loc
  -> Text
  -> TC l (TypedConst l t)
typecheckIdent ty loc ident = do
  name <- mkThriftName ident
  (thistrueTy, renamed, locDefined) <- lookupConst name loc
  Env{..} <- ask
  case thistrueTy of
    Some trueTy -> case trueTy `eqOrAlias` ty of
      Just Refl -> pure $ Identifier renamed ty locDefined
      Nothing   ->
        if not (optsLenient options) then
          typeError loc $ IdentMismatch ty trueTy name
        else case useEnumAsInt trueTy ty of
          Nothing -> typeError loc $ IdentMismatch ty trueTy name
          Just trueTyEnum ->
            -- This is the one place where WeirdEnumToInt is constructed
            pure $ WeirdEnumToInt ty renamed trueTyEnum locDefined

-- | helper for 'typecheckIdent' in weird case
useEnumAsInt
  :: Typecheckable l
  => Type l u -- ^ @t1@ : Checked againt TEnum, type of default or const value
  -> Type l v -- ^ @t2@ : Checked against int types, type of target
  -> Maybe (Type l EnumVal) -- ^ When Just this is a type-refined @t1@
useEnumAsInt t1 t2 = case t1 of
  TEnum{} -> case t2 of
    I8 -> Just t1
    I16 -> Just t1
    I32 -> Just t1
    I64 -> Just t1
    _ -> Nothing
  _ -> Nothing

-- This tells if the two types are compatible. This means that they are either
-- structurally equal or aliases
-- NOTE: using annotations, you can create code that won't typecheck in the
-- haskell compiler, but will typecheck in fbthrift
eqOrAlias :: Typecheckable l => Type l u -> Type l v -> Maybe (u :~: v)
-- Typedefs are equal if they are aliases of the other type
eqOrAlias (TTypedef _ u _loc) v = eqOrAlias u v
eqOrAlias u (TTypedef _ v _loc) = eqOrAlias u v
-- Newtypes may be lifted
eqOrAlias (TNewtype n1 u _loc1) (TNewtype n2 v _loc2)
  | n1 == n2  = apply Refl <$> eqOrAlias u v
  | otherwise = Nothing
eqOrAlias TNewtype{} _ = Nothing
-- Base types only equal themselves
eqOrAlias I8  I8  = Just Refl
eqOrAlias I8  _   = Nothing
eqOrAlias I16 I16 = Just Refl
eqOrAlias I16 _   = Nothing
eqOrAlias I32 I32 = Just Refl
eqOrAlias I32 _   = Nothing
eqOrAlias I64 I64 = Just Refl
eqOrAlias I64 _   = Nothing
eqOrAlias TFloat TFloat = Just Refl
eqOrAlias TFloat _      = Nothing
eqOrAlias TDouble TDouble = Just Refl
eqOrAlias TDouble _       = Nothing
eqOrAlias TBool TBool = Just Refl
eqOrAlias TBool _     = Nothing
eqOrAlias TText TText = Just Refl
eqOrAlias TText _     = Nothing
eqOrAlias TBytes TBytes = Just Refl
eqOrAlias TBytes _      = Nothing
-- Recursive types must be deeply equal
eqOrAlias (TSet u) (TSet v) = apply Refl <$> eqOrAlias u v
eqOrAlias TSet{} _ = Nothing
eqOrAlias (THashSet u) (THashSet v) = apply Refl <$> eqOrAlias u v
eqOrAlias THashSet{} _ = Nothing
eqOrAlias (TList u) (TList v) = apply Refl <$> eqOrAlias u v
eqOrAlias TList{} _ = Nothing
eqOrAlias (TMap k1 v1) (TMap k2 v2) =
  apply <$> (apply Refl <$> eqOrAlias k1 k2) <*> eqOrAlias v1 v2
eqOrAlias TMap{} _ = Nothing
eqOrAlias (THashMap k1 v1) (THashMap k2 v2) =
  apply <$> (apply Refl <$> eqOrAlias k1 k2) <*> eqOrAlias v1 v2
eqOrAlias THashMap{} _ = Nothing
-- Named types must have the same name
eqOrAlias (TStruct n1 _loc1) (TStruct n2 _loc2) | n1 == n2 = Just Refl
eqOrAlias TStruct{} _ = Nothing
eqOrAlias (TException n1 _loc1) (TException n2 _loc2) | n1 == n2 = Just Refl
eqOrAlias TException{} _ = Nothing
eqOrAlias (TUnion n1 _loc1) (TUnion n2 _loc2) | n1 == n2 = Just Refl
eqOrAlias TUnion{} _ = Nothing
eqOrAlias (TEnum n1 _loc1 _) (TEnum n2 _loc2 _) | n1 == n2 = Just Refl
eqOrAlias TEnum{} _ = Nothing
eqOrAlias (TSpecial u) (TSpecial v) = eqSpecial u v
eqOrAlias TSpecial{} _ = Nothing

-- Note that the type parameters are the same here. This is very important as
-- the aliased type must be equal to the original type
getAliasedType :: Type l t -> Type l t
getAliasedType (TTypedef _ ty _loc) = getAliasedType ty
getAliasedType ty = ty

mkFieldMap
  :: [ListElem MapPair Loc]
  -> TC l (Map.Map Text (UntypedConst Loc))
mkFieldMap = fmap Map.fromList . traverse getName
  where
    getName ListElem{ leElem = MapPair{..} } = case mpKey of
      UntypedConst _ (StringConst s _) -> pure (s, mpVal)
      val@UntypedConst{..} -> typeError (lLocation ucLoc) $ InvalidField val

mkStructFieldMap
  :: [ListElem StructPair Loc]
  -> TC l (Map.Map Text (UntypedConst Loc))
mkStructFieldMap = fmap Map.fromList . traverse getName
  where
    getName ListElem{ leElem = StructPair{..} } =
      pure (spKey, spVal)

-- Given a schema, this function proves that the resulting
-- StructVal adheres to that schema
typecheckStruct
  :: Typecheckable l
  => Loc
  -> Schema l s
  -> Map.Map Text (UntypedConst Loc)
  -> TC l (StructVal l s)

-- Empty schema only matches empty map
typecheckStruct loc SEmpty fields =
  case Map.keys fields of
    [] -> pure Empty
    k : _ -> typeError loc $ UnknownField k

-- Default Field
typecheckStruct loc (SField proxy fname ty schema) fields =
  case Map.lookup fname fields of
    Nothing ->
      ConsDefault proxy ty <$> typecheckStruct loc schema fields
    Just val -> ConsVal proxy ty
                <$> typecheckConst ty val
                <*> typecheckStruct loc schema (Map.delete fname fields)

-- Required Field must be present
typecheckStruct loc (SReqField proxy fname ty schema) fields =
  case Map.lookup fname fields of
    Nothing  -> typeError loc $ MissingField fname
    Just val -> ConsVal proxy ty
                <$> typecheckConst ty val
                <*> typecheckStruct loc schema (Map.delete fname fields)

-- Optional field may be missing
typecheckStruct loc (SOptField proxy fname ty schema) fields =
  mkCons
  <$> sequence (typecheckConst ty <$> Map.lookup fname fields)
  <*> typecheckStruct loc schema (Map.delete fname fields)
  where
    mkCons (Just val) struct = ConsJust proxy ty val struct
    mkCons Nothing struct = ConsNothing proxy struct

typecheckUnion
  :: Typecheckable l
  => USchema l s
  -> Text
  -> UntypedConst Loc
  -> TC l (UnionVal l s)
typecheckUnion SEmpty name UntypedConst{..} =
  typeError (lLocation ucLoc) $ UnknownField name
typecheckUnion (SReqField proxy fname ty schema) name val
  | fname == name = do
    tval <- typecheckConst ty val
    pure $ UnionVal proxy ty tval PHere
  | otherwise = do
    UnionVal p t v proof <- typecheckUnion schema name val
    pure $ UnionVal p t v (PThere proof)

literal :: t -> TC l (TypedConst l t)
literal = pure . Literal

insertContext
  :: Loc
  -> Text -- ^ Thrift Name
  -> Text -- ^ Renamed Name
  -> a
  -> Context a
  -> Either [TypeError l] (Context a)
insertContext loc tName lName v ctx@Context{..}
  | Map.member tName cMap ||
    Set.member lName cScope = Left [TypeError loc $ DuplicateName tName]
  | otherwise = Right $ ctx { cMap   = Map.insert tName v cMap
                            , cScope = Set.insert lName cScope
                            }
insertUnique
  :: Loc -> Text -> a -> Map.Map Text a -> Either [TypeError l] (Map.Map Text a)
insertUnique loc k v m
  | Map.member k m = Left [TypeError loc $ DuplicateName k]
  | otherwise      = Right $ Map.insert k v m

addToScope :: Loc -> Text -> Context a -> Either [TypeError l] (Context a)
addToScope loc x ctx@Context{..}
  | Set.member x cScope = Left [TypeError loc $ DuplicateName x]
  | otherwise = Right ctx { cScope = Set.insert x cScope }

-- Resolve Named Types ---------------------------------------------------------

-- Create a map from all named types to their resolved type

mkTypemap
  :: forall l. Typecheckable l
  => (Text, Options l)
  -> ImportMap l
  -> [Parsed Decl]
  -> Either [TypeError l] (TypeMap l)
mkTypemap (thriftName, opts) imap =
    foldM resolve emptyContext <=< sortDecls
  where
    resolve :: TypeMap l -> Parsed Decl -> Either [TypeError l] (TypeMap l)
    resolve m (D_Typedef t@Typedef{..}) = do
      tdef <- mkTypedef <$> runTypechecker env (resolveAnnotatedType tdType)
      insertContext loc tdName hsname tdef m
        where
          loc = lLocation (tdlName tdLoc)
          env = (emptyEnv (thriftName, opts))
            { typeMap   = m
            , importMap = imap
            }
          uname = mkName tdName hsname
          hsname = renameTypedef opts t
          mkTypedef :: Some (Type l) -> Some (Type l)
          mkTypedef (Some u)
            | isNewtype (getAnns tdAnns) = Some $ TNewtype uname u loc
            -- Other annotations will be checked in resolveTypedef
            | otherwise = Some $ TTypedef uname u loc
    resolve m (D_Struct s@Struct{..}) =
      insertContext (lLocation $ slName structLoc) structName hsname structTy m
        where
          loc = lLocation (slName structLoc)
          uname = mkName structName hsname
          hsname = renameStruct opts s
          structTy =
            case structType of
              StructTy    -> Some $ TStruct uname loc
              ExceptionTy -> Some $ TException uname loc
    resolve m (D_Union u@Union{..}) =
      insertContext (lLocation $ slName unionLoc) unionName hsname
      (Some $ TUnion uname loc) m
      where
        loc = lLocation (slName unionLoc)
        uname = mkName unionName hsname
        hsname = renameUnion opts u
    resolve m (D_Enum e@Enum{..}) =
      insertContext (lLocation $ slName enumLoc) enumName (renameEnum opts e)
      (getEnumType opts e) m
    resolve m D_Const{} = pure m
    resolve m D_Service{} = pure m
    resolve m D_Interaction{} = pure m

-- Topologically sort the Decls based on dependencies
-- This function will fail if there is a cycle
sortDecls :: [Parsed Decl] -> Either [TypeError l] [Parsed Decl]
sortDecls decls = traverse getVertex sccs
  where
    sccs  = stronglyConnComp graph
    graph = mapMaybe mkVertex decls
    -- Convert a Decl into a vertex in the graph
    mkVertex d@(D_Typedef Typedef{..}) = Just (d, tdName, getEdges tdType)
    mkVertex d@(D_Struct Struct{..})   = Just (d, structName, [])
    mkVertex d@(D_Union Union{..})     = Just (d, unionName, [])
    mkVertex d@(D_Enum Enum{..})       = Just (d, enumName, [])
    mkVertex D_Const{}   = Nothing
    mkVertex D_Service{} = Nothing
    mkVertex D_Interaction{} = Nothing
    -- Find the dependencies of a type
    getEdges :: AnnotatedType Loc t -> [Text]
    getEdges AnnotatedType{..} =
      case atType of
        TNamed name -> [name]
        TSet u      -> getEdges u
        TList u     -> getEdges u
        TMap k v    -> getEdges k ++ getEdges v
        _           -> []
    -- Get the vertex our of an SCC
    getVertex (AcyclicSCC v) = Right v
    getVertex (CyclicSCC vs@(d:_)) =
      Left [TypeError (getLoc d) $ CyclicTypes vs]
    -- This case should not be possible, but we are using an external library,
    -- so there is no type-level way to enforce it
    getVertex (CyclicSCC []) = Left []

resolveAnnotatedType
  :: forall l t. Typecheckable l
  => AnnotatedType Loc t
  -> TC l (Some (Type l))
resolveAnnotatedType AnnotatedType{..} =
  resolveType atType atLoc atAnnotations

resolveType
  :: forall l t n. Typecheckable l
  => TType 'Unresolved () Loc t
  -> TypeLoc n Loc
  -> Maybe (Annotations Loc)
  -> TC l (Some (Type l))
resolveType atType atLoc atAnnotations =
  case atType of
    -- Resolving base types is trivial
    I8      -> annotate I8
    I16     -> annotate I16
    I32     -> annotate I32
    I64     -> annotate I64
    TDouble -> annotate TDouble
    TFloat  -> annotate TFloat
    TText   -> annotate TText
    TBytes  -> annotate TBytes
    TBool   -> annotate TBool
    -- Rescursive types must be resolved recursively
    TSet u     -> resolve TSet u
    THashSet u -> resolve THashSet u
    TList u    -> resolve TList u
    TMap k v -> do
      thisrk <- resolveAnnotatedType k
      thisrv <- resolveAnnotatedType v
      case (thisrk, thisrv) of
        (Some rk, Some rv) -> annotate $ TMap rk rv
    THashMap k v -> do
      thisrk <- resolveAnnotatedType k
      thisrv <- resolveAnnotatedType v
      case (thisrk, thisrv) of
        (Some rk, Some rv) -> annotate $ THashMap rk rv
    -- Named type may not be resolvable
    TNamed name -> do
      qname <- mkThriftName name
      lookupType qname (getTypeLoc atLoc)
  where
    annotate :: Type l u -> TC l (Some (Type l))
    annotate ty = resolveTypeAnnotations ty $ getAnns atAnnotations

    resolve
      :: (forall v. Type l v -> Type l (s v))
      -> AnnotatedType Loc u
      -> TC l (Some (Type l))
    resolve mkType u = resolveAnnotatedType u >>=
      \(Some v) -> annotate (mkType v)

mkThriftName :: Text -> TC l ThriftName
mkThriftName text
  | Text.null suff = pure $ UName pre -- obviously undotted unqualified name
  | otherwise = do
      Env{..} <- ask
      if
        | Map.member pre importMap ->
            pure $ QName pre (Text.drop 1 suff) -- directly included qname
        | not (optsLenient options) ->
            pure $ UName text -- pass through the dotted name, might be enum
        | envName /= pre ->
            pure $ UName text -- pass through the dotted name, might be enum
        | otherwise ->
            -- some weird code uses its own filename as qualifier
            -- handle this by pretending the unwanted 'pre' is not there
            pure $ UName (Text.drop 1 suff)
  where (pre, suff) = Text.breakOn "." text


-- Build Schemas ---------------------------------------------------------------

mkSchemaMap
  :: Typecheckable l
  => (Text, Options l)
  -> ImportMap l
  -> TypeMap l
  -> [Parsed Struct]
  -> Either [TypeError l] (SchemaMap l)
mkSchemaMap (thriftName, opts) imap tmap = foldM insertSchema Map.empty
  where
    insertSchema m s@Struct{..} = do
      ty <- runTypechecker env (mkSchema s)
      insertUnique (lLocation $ slName structLoc) structName ty m
      where
        env = (emptyEnv (thriftName, opts))
                { typeMap   = tmap
                , schemaMap = m
                , importMap = imap
                }

mkSchema :: Typecheckable l => Parsed Struct -> TC l (Some (Schema l))
mkSchema Struct{..} = buildSchema structMembers
  where
    buildSchema (field@Field{..} : fields) = do
      (rty, tschema) <- (,)
        <$> resolveAnnotatedType fieldType
        <*> buildSchema fields
      opts <- options <$> ask
      let renamed =
            Text.unpack $ renameField opts (getAnns structAnns) structName field
      case (someSymbolVal renamed, rty, tschema) of
       (SomeSymbol proxy, Some ty, Some schema) ->
          case fieldRequiredness of
            Default ->
              pure . Some $
              SField proxy fieldName ty schema
            Optional{} -> pure . Some $ SOptField proxy fieldName ty schema
            Required{} -> pure . Some $ SReqField proxy fieldName ty schema
    buildSchema [] = pure $ Some SEmpty

mkUnionMap
  :: Typecheckable l
  => (Text, Options l)
  -> ImportMap l
  -> TypeMap l
  -> [Parsed Union]
  -> Either [TypeError l] (UnionMap l)
mkUnionMap (thriftName, opts) imap tmap  = foldM insertSchema Map.empty
  where
    insertSchema m u@Union{..} = do
      schema <- runTypechecker env (mkUSchema u)
      insertUnique (lLocation $ slName unionLoc) unionName schema m
      where env = (emptyEnv (thriftName, opts))
                  { typeMap   = tmap
                  , unionMap  = m
                  , importMap = imap
                  }

mkUSchema
  :: Typecheckable l => Parsed Union -> TC l (Some (USchema l))
mkUSchema u@Union{..} =
  foldM buildSchema (Some SEmpty) unionAlts
  where
    buildSchema (Some schema) alt@UnionAlt{..} = do
      rty <- resolveAnnotatedType altType
      opts <- options <$> ask
      let renamed = Text.unpack $ renameUnionAlt opts u alt
      case (someSymbolVal renamed, rty) of
        (SomeSymbol proxy, Some ty) ->
          pure . Some $ SReqField proxy altName ty  schema

-- We need the enums to be resolved in order to build the map because we need to
-- know the numeric values for each field
mkEnumMap :: Typecheckable l => Options l -> [Parsed Enum] -> EnumMap
mkEnumMap opts = Map.fromList . map mkAssoc
  where
    mkAssoc :: Parsed Enum -> (Text, EnumValues)
    mkAssoc e@Enum{..} = (enumName, enumValues)
      where
        enumValues =
          ( Map.fromList
            [(evValue, (rename evName, lLocation (evlName evLoc)))
            | EnumValue{..} <- enumConstants]
          , Map.fromList
            [ (evName, (rename evName, lLocation (evlName evLoc)))
            | EnumValue{..} <- enumConstants
            ]
          )
        rename name = mkName name $ renameEnumAlt opts e name

-- Build EnumInt Map -----------------------------------------------------------

-- | When lenient mode is active (argument @--lenient@) then capture the enum
-- values into 'imap' so they can be used as default integer values.
mkEnumInt:: Typecheckable l => Options l -> [Parsed Enum] -> EnumInt
mkEnumInt opts
    | optsLenient opts = Map.unionsWith safe . map mkOne
    | otherwise = mempty
  where
    mkOne :: Parsed Enum -> EnumInt
    mkOne Enum{..} = Map.fromListWith safe
      [ (evName, Just evValue)
      | EnumValue{..} <- enumConstants
      ]

    safe (Just new) j@(Just old) | new == old = j -- collision but no ambiguity
    safe _new _old = Nothing -- collision with ambiguity, set to Nothing

-- Build Service Map -----------------------------------------------------------

mkServiceMap
  :: Typecheckable l
  => (Text, Options l)
  -> ImportMap l
  -> [Parsed Service]
  -> Either [TypeError l] ServiceMap
mkServiceMap (thriftName, opts) imap =
    foldM addToMap Map.empty <=< sortServices
  where
    addToMap ctx s@Service{..} = do
      scope <- mkScope ctx s
      let renamed = mkName serviceName $ renameService opts s
          loc = sloc serviceLoc
      insertUnique loc serviceName (renamed, scope, loc) ctx
    addToSet loc x s
      | Set.member x s = Left [TypeError loc (DuplicateName x)]
      | otherwise      = pure $ Set.insert x s
    mkScope ctx Service{..} = do
      let env = (emptyEnv (thriftName, opts))
                  { importMap = imap
                  , serviceMap = ctx
                  }
      scope <-
        case serviceSuper of
          Nothing -> pure Set.empty
          Just Super{..} -> runTypechecker env $ do
            name <- mkThriftName supName
            (\(_, setFuns, _) -> setFuns) <$>
              lookupService name (sloc serviceLoc)
      foldM insFunc scope serviceStmts
    insFunc ctx (FunctionStmt f@Function{..}) =
      addToSet (lLocation $ fnlName funLoc) renamed ctx
      where renamed = renameFunction opts f
    insFunc ctxt _ = Right ctxt
    sloc = lLocation . slName

sortServices
  :: [Parsed Service] -> Either [TypeError l] [Parsed Service]
sortServices services = traverse getVertex sccs
  where
    sccs  = stronglyConnComp graph
    graph = map mkVertex services
    mkVertex s@Service{..} =
      (s, serviceName, maybeToList $ supName <$> serviceSuper)
    getVertex (AcyclicSCC s) = pure s
    getVertex (CyclicSCC ss@(Service{..}:_)) =
      Left [TypeError (lLocation $ slName serviceLoc) (CyclicServices ss)]
    getVertex (CyclicSCC []) = Left []
