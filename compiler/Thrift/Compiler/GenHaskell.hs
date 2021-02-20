-- Copyright (c) Facebook, Inc. and its affiliates.

module Thrift.Compiler.GenHaskell
  ( genHsCode
  , writeHsCode
  , writeModule, showModule
  , ThriftModule(..)
  , commonPragmas
  ) where

import Data.List
import Data.List.Extra
import Data.Text (Text)
import Language.Haskell.Exts hiding (parse, Decl, name, app)
import System.Directory
import System.FilePath
import Text.Printf
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Language.Haskell.Exts.Syntax as HS

import Thrift.Compiler.GenClient
import Thrift.Compiler.GenConst
import Thrift.Compiler.GenEnum
import Thrift.Compiler.GenFunction
import Thrift.Compiler.GenService
import Thrift.Compiler.GenStruct
import Thrift.Compiler.GenTypedef
import Thrift.Compiler.GenUnion
import Thrift.Compiler.GenUtils

import Thrift.Compiler.Options
import Thrift.Compiler.Plugins.Haskell
import Thrift.Compiler.Types as Thrift hiding (noLoc)

data InstancesFile a = InstancesFile
  { ifPragmas :: [ModulePragma a]
  , ifImports :: [ImportDecl a]
  , ifDecls   :: [HS.Decl a]
  }

data ThriftModule = ThriftModule
  { tmPath :: FilePath
  , tmContents :: String
  , tmModuleName :: String
  }

writeHsCode :: Options Haskell -> Program Haskell Thrift.Loc -> IO [FilePath]
writeHsCode opts prog =
  -- Write the Generated Files
  mapM writeModule =<< genHsCode opts prog

genHsCode :: Options Haskell -> Program Haskell Thrift.Loc -> IO [ThriftModule]
genHsCode Options{..} prog@Program{..} = do
  let
    progHSPath = Text.unpack $ Text.replace "." "/" progHSName
    HsOpts{..} = optsLangSpecific
    dir = progOutPath </> hsoptsGenPrefix </> progHSPath
  relativeDir <- makeRelativeToCurrentDirectory dir

  let
    (typesModuleName, typesModuleBase) =
      genTypesModule prog hsoptsExtensions hsoptsExtraHasFields
    -- Get instances file if it exists
    typesModuleBody = case progInstances of
      Just (Module _ _ pragmas imports decls) ->
        showModuleWithInstances (relativeDir </> "Types.hs") typesModuleBase $
        InstancesFile pragmas imports decls
      _ -> showThriftModule typesModuleBase

  return $
    ThriftModule
      (dir </> "Types.hs")
      typesModuleBody
      typesModuleName :
    concat
      [ [ ThriftModule
          (dir </> Text.unpack serviceName </> "Client.hs")
          (showThriftModule clientModule)
          clientModuleName
        , ThriftModule
          (dir </> Text.unpack serviceName </> "Service.hs")
          (showThriftModule serviceModule)
          serviceModuleName
        ]
      | D_Service s@Service{..} <- progDecls
      , let
          (clientModuleName, clientModule) = genClientModule prog s
          (serviceModuleName, serviceModule) = genServiceModule prog s
      ]

genTypesModule
  :: Program Haskell Thrift.Loc -> [Text] -> Bool -> (String, Module ())
genTypesModule prog@Program{..} extensions extraHasFields =
  genModule prog "Types" pragmas
    (concat exports)
    (map genImportModule $ Set.toList imports)
    (concat decls)
  where
    pragmas = commonPragmas (options progEnv) ++
              map (LanguagePragma () . (:[]) . textToName)
                (("GeneralizedNewtypeDeriving" : extensions) ++
                (if extraHasFields then hasFieldsExtensions else []))

    hasFieldsExtensions =
      [ "DataKinds"
      , "FlexibleInstances"
      , "MultiParamTypeClasses"
      ]

    (decls, imports, exports) = foldr genDecl ([], baseImports, []) progDecls
    baseImports = Set.fromList $
      [ QImport "Thrift.CodegenTypesOnly" "Thrift"
      , QImport "Prelude" "Prelude"
      ] ++
      map importFromInclude progIncludes

    genDecl decl (ds, is, es) = (d : ds, Set.union i is, e : es)
      where
        (d, i, e) = case decl of
          D_Struct s ->
            ( genStructDecl extraHasFields s
            , genStructImports s
            , [structExport s]
            )
          D_Union u ->
            (genUnionDecl u, genUnionImports u, [unionExport u])
          D_Typedef t ->
            (genTypedefDecl t, genTypedefImports t, [tdefExport t])
          D_Enum en -> (genEnumDecl en, genEnumImports, enumExport en)
          D_Const c -> (genConstDecl c, genConstImports c, [constExport c])
          -- Services are not included in this module
          D_Service{} -> mempty

        tdefExport Typedef{..} = case tdTag of
          IsNewtype -> newtypeExport tdResolvedName
          IsTypedef -> EAbs () (NoNamespace ()) $ unqualSym tdResolvedName
        newtypeExport name =
          EThingWith () (NoWildcard ()) (unqualSym name)
          [ ConName () (textToName name)
          , VarName () (textToName $ "un" <> name)
          ]
        structExport :: HS Struct -> ExportSpec ()
        structExport Struct{..} =
          EThingWith () (NoWildcard ()) (unqualSym structResolvedName) $
          ConName () (textToName structResolvedName) :
          [ VarName () (textToName fieldResolvedName)
          | Field{..} <- structMembers ]
        unionExport :: HS Union -> ExportSpec ()
        unionExport Union{..} =
          EThingWith () (NoWildcard ()) (unqualSym unionResolvedName) $
          (case unionHasEmpty of
             HasEmpty -> (ConName () (textToName unionEmptyName) :)
             NonEmpty -> id)
          [ ConName () (textToName altResolvedName) | UnionAlt{..} <- unionAlts ]
        enumExport :: HS Thrift.Enum -> [ExportSpec ()]
        enumExport Enum{..}
          | enumIsPseudo =
              newtypeExport enumResolvedName :
              [ EVar () $ unqualSym $ evResolvedName
              | EnumValue{..} <- enumConstants
              ]
          | otherwise =
            [ EThingWith () (NoWildcard ()) (unqualSym enumResolvedName) $
              [ConName () (textToName evResolvedName)
              | EnumValue{..} <- enumConstants
              ] ++
              [ConName () (textToName $ enumResolvedName <> "__UNKNOWN")
              | not enumNoUnknown
              ]
            ]
        constExport = EVar () . unqualSym . constResolvedName

genClientModule :: Program Haskell Thrift.Loc -> HS Service -> (String, Module ())
genClientModule prog@Program{..} service@Service{..} =
  genModule prog (serviceName <> ".Client") pragmas exports imports decls
  where
    pragmas = commonPragmas opts ++
              map (LanguagePragma () . (:[]) . textToName)
              (["FlexibleContexts", "TypeFamilies", "TypeOperators"] ++
               hsoptsExtensions optsLangSpecific)
    exports = EAbs () (NoNamespace ()) (unqualSym serviceResolvedName) :
              concatMap
              (\Function{..} ->
                map (EVar () . unqualSym . ($ funResolvedName)) $
                [ id, (<> "IO"), ("send_" <>), ("_build_" <>) ] ++
                (if funIsOneWay then [] else [ ("recv_" <>), ("_parse_"<>) ]))
              serviceFunctions
    imports = map genImportModule $ Set.toList $ Set.unions $
              Set.singleton (QImport "Thrift.Codegen" "Thrift") :
              Set.singleton (TypesImport progHSName) :
              Set.fromList (map importFromInclude progIncludes) :
              genClientImports progHSName service :
              map genFunctionImports serviceFunctions
    decls   = genClientDecls service ++
              concatMap (genFunctionDecls service) serviceFunctions
    opts@Options{..} = options progEnv

genServiceModule :: Program Haskell Thrift.Loc -> HS Service -> (String, Module ())
genServiceModule prog@Program{..} service@Service{..} =
  genModule prog (serviceName <> ".Service") pragmas exports imports decls
  where
    pragmas = commonPragmas (options progEnv) ++
              [ LanguagePragma () [textToName "GADTs"]
              ]
    exports = genServiceExports service
    imports = map genImportModule $ Set.toList $ Set.unions $
              Set.singleton (QImport "Thrift.Codegen" "Thrift") :
              Set.singleton (QImport (progHSName <> ".Types") "Types") :
              Set.fromList (map importFromInclude progIncludes) :
              [genServiceImports progHSName service]
    decls   = genServiceDecls service

commonPragmas :: Options Haskell -> [ModulePragma ()]
commonPragmas Options{ optsLangSpecific = HsOpts{..} } =
  (if hsoptsDupNames
   then (LanguagePragma () [textToName "DuplicateRecordFields"] :)
   else id)
  [ LanguagePragma () [textToName "OverloadedStrings"]
  , LanguagePragma () [textToName "BangPatterns"]
  , OptionsPragma () (Just GHC) "-fno-warn-unused-imports"
  , OptionsPragma () (Just GHC) "-fno-warn-overlapping-patterns"
  , OptionsPragma () (Just GHC) "-fno-warn-incomplete-patterns"
  ]

genModule
  :: Program Haskell Thrift.Loc
  -> Text
  -> [ModulePragma ()]
  -> [ExportSpec ()]
  -> [ImportDecl ()]
  -> [HS.Decl ()]
  -> (String, Module ())
genModule Program{..} moduleName pragmas exports imports decls =
  ( fullName
  , Module ()
      (Just
       (ModuleHead () (ModuleName () fullName)
        Nothing
        (Just $ ExportSpecList () exports)))
      pragmas
      imports
      decls
  )
  where
    fullName = Text.unpack $ progHSName <> "." <> moduleName

writeModule :: ThriftModule -> IO FilePath
writeModule ThriftModule{..} = do
  let (dir, _fname) = breakOnEnd "/" tmPath
  createDirectoryIfMissing True dir
  writeFile tmPath tmContents
  return tmPath

showThriftModule :: Module () -> String
showThriftModule = showModule autogenComment

showModule :: String -> Module () -> String
showModule header = (header ++) . prettyPrintWithMode baseMode . (noLoc <$)

showModuleWithInstances
  :: FilePath
  -> Module ()
  -> InstancesFile SrcSpanInfo
  -> String
showModuleWithInstances path baseModule InstancesFile{..} = fileBody
  where
    -- Put everything together
    fileBody = foldl1 catWithPragma
               [ fileHeader, fileImports, fileDecls ]
    catWithPragma a b = a ++ linePragma (length (lines a)) ++ b
    linePragma n = printf "{-# LINE %d \"%s\" #-}\n" (n + 2) path

    fileHeader = autogenComment ++ extraPragmas
    fileImports = unlines imports ++ extraImports
    fileDecls   = unlines decls ++ extraDecls

    (imports, decls) = splitAt lastImport baseFile
    baseFile = lines $ prettyPrintWithMode baseMode $ noLoc <$ baseModule

    lenBase = length baseFile
    lastImport = maybe lenBase (lenBase -) $
                 findIndex (isPrefixOf "import") $ reverse baseFile
    -- Pretty Print the extras
    extraPragmas = annot ifPragmas
    extraImports = annot ifImports
    extraDecls   = annot ifDecls
    annot xs = unlines $ map pp xs

    pp :: (HS.Annotated s, Pretty (s SrcSpanInfo))
       => s SrcSpanInfo -> String
    pp d =
      printf "{-# LINE %d \"%s\" #-}\n" srcSpanStartLine srcSpanFilename
       ++ prettyPrintWithMode baseMode d
      where
        SrcSpanInfo SrcSpan{..} _ = ann d

autogenComment :: String
autogenComment = unlines
  [ "-----------------------------------------------------------------"
  , "-- Autogenerated by Thrift"
  , "--"
  , "-- DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING"
  , "--  @" ++ "generated"
  , "-----------------------------------------------------------------"
  ]

baseMode :: PPHsMode
baseMode = defaultMode
  { classIndent   = 2
  , doIndent      = 3
  , multiIfIndent = 3
  , caseIndent    = 2
  , letIndent     = 2
  , whereIndent   = 2
  , onsideIndent  = 2
  , spacing       = True
  , layout        = PPOffsideRule
  }
