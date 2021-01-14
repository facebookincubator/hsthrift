-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

module Thrift.Compiler.GenService
  ( genServiceDecls
  , genServiceImports
  , genServiceExports
  ) where

#if __GLASGOW_HASKELL__ > 804
#define This Some
#define THIS "Some"
#else
#define THIS "This"
#endif

import Control.Monad
#if __GLASGOW_HASKELL__ <= 804
import Data.Monoid ((<>))
#endif
import qualified Data.Set as Set
import Data.Some
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.Exts.Syntax as HS

import Thrift.Compiler.GenStruct
import Thrift.Compiler.GenUtils
import Thrift.Compiler.Plugins.Haskell
import Thrift.Compiler.Types hiding (Decl(..))


-- | All things required to generate a Service.hs file

genServiceExports :: HS Service -> [HS.ExportSpec ()]
genServiceExports s@Service{..} =
  HS.EThingWith () (EWildcard () 0) (unqualSym $ commandTypeName s) []
  : map (HS.EVar () . UnQual () . Ident ())
    ["reqName'", "reqParser'", "respWriter'", "onewayFunctions'"]

genServiceImports :: Text.Text -> HS Service -> Set.Set Import
genServiceImports this Service{..} =
  foldr (Set.union . importsForFunc) baseImports serviceFunctions
  where
    importsForFunc Function{..} =
      foldr (Set.union . getImports) (retImport funResolvedType) funArgs
      where
        retImport Nothing = Set.empty
        retImport (Just (This i)) = typeToImport i

    getImports :: HS (Field u) -> Set.Set Import
    getImports Field{..} = typeToImport fieldResolvedType

    baseImports = Set.fromList
      [ QImport "Prelude" "Prelude"
      , QImport "Control.Exception" "Exception"
      , QImport "Control.Monad.ST.Trans" "ST"
      , QImport "Control.Monad.Trans.Class" "Trans"
      , QImport "Data.ByteString.Builder" "Builder"
      , QImport "Data.Default" "Default"
      , QImport "Data.HashMap.Strict" "HashMap"
      , QImport "Data.Int" "Int"
      , QImport "Data.Proxy" "Proxy"
      , QImport "Data.Text" "Text"
      , QImport "Thrift.Binary.Parser" "Parser"
      , QImport "Thrift.Protocol.ApplicationException.Types" "Thrift"
      , QImport "Thrift.Processor" "Thrift"
      , SymImport "Prelude"
        [ "<$>", "<*>", "++", ".", "==" ]
      , SymImport "Control.Applicative" [ "<*", "*>" ]
      , SymImport "Data.Monoid" [ "<>" ]
      ] `Set.union`
      (case resolvedName . fst . supResolvedName <$> serviceSuper of
        Nothing -> Set.empty
        Just (UName n) -> mkImport this n
        Just (QName m n) -> mkImport m n)
    mkImport m n =
      Set.singleton $ QImport (Text.intercalate "." [m, n, "Service"]) n

genServiceDecls :: HS Service -> [HS.Decl ()]
genServiceDecls s@Service{..} =
  [ genCommandDT s
  , processorInstance
  ] ++ concat
  [ genReqName s
  , genReqParser s
  , genRespWriter s
  , genOneWays s
  ]

  where
    processorInstance = HS.InstDecl () Nothing
      (HS.IRule () Nothing Nothing $
       HS.IHApp ()
         (HS.IHCon () $ qualSym "Thrift" "Processor")
         (HS.TyCon () $ unqualSym $ commandTypeName s)
      )
      (Just $ (flip map
         [ "reqName"
         , "reqParser"
         , "respWriter"
         ] $ \fn ->
         HS.InsDecl () $ HS.FunBind () [ HS.Match () (textToName fn) []
           (HS.UnGuardedRhs () $ con (fn <> "'")) Nothing
         ]) ++ [
         HS.InsDecl () $ HS.FunBind () [
           HS.Match () (textToName "onewayFns") [HS.PWildCard ()]
             (HS.UnGuardedRhs () $ var "onewayFunctions'") Nothing
         ]])

-- | Generates a GADT for all functions that the service can implement
-- If extending a service, adds a "Super<Name>" constructor to forward
-- requests along
genCommandDT :: HS Service -> HS.Decl ()
genCommandDT s@Service{..} = HS.GDataDecl () (HS.DataType ()) Nothing
  (HS.DHApp ()
     (HS.DHead () (textToName $ commandTypeName s))
     (HS.UnkindedVar () (HS.Ident () "a")))
  Nothing
  genFunctions
  mzero
  where
    genFunctions = map genDTFunction serviceFunctions ++
      case fst . supResolvedName <$> serviceSuper of
        Nothing -> []
        Just Name{..} -> [genSuper $ localName resolvedName]

    genSuper superName =
      HS.GadtDecl () (textToName $ "Super" <> superName)
#if MIN_VERSION_haskell_src_exts(1,21,0)
        Nothing Nothing
#endif
        Nothing $
        HS.TyFun ()
        (qualType superName (superName <> "Command") `appT` simpleType "a")
        (simpleType (commandTypeName s) `appT` tvar "a")

    genDTFunction Function{..} =
      HS.GadtDecl () (textToName $ toCamel funName)
#if MIN_VERSION_haskell_src_exts(1,21,0)
        Nothing Nothing
#endif
        Nothing (mkArgs funArgs)
      where
        mkArgs = foldr
          (\Field{..} ->
            HS.TyFun () $ genType $ qualifyType "Types" fieldResolvedType)
          (simpleType (commandTypeName s) `appT`
            maybe (HS.unit_tycon ())
            (`withSome` (genType . qualifyType "Types")) funResolvedType)

-- | Generates a function that returns a Text name for the given function
genReqName :: HS Service -> [HS.Decl ()]
genReqName s@Service{..} =
  [ HS.TypeSig () [textToName "reqName'"] $ HS.TyFun ()
      (HS.TyApp () (simpleType $ commandTypeName s) (simpleType "a"))
      (qualType "Text" "Text")
  ] ++ map genFunction serviceFunctions ++
    case fst . supResolvedName <$> serviceSuper of
      Nothing -> noParentBody (null serviceFunctions)
      Just Name{..} -> [genSuper $ localName resolvedName]

  where
    genSuper superName = HS.FunBind ()
      [ HS.Match () (textToName "reqName'")
        [HS.PParen () $ HS.PApp ()
           (HS.UnQual () $ textToName $ "Super" <> superName) [pvar "x"]]
        (HS.UnGuardedRhs () $ qvar superName "reqName'" `app` var "x")
        Nothing
      ]

    genFunction Function{..} = HS.FunBind ()
      [ HS.Match () (textToName "reqName'")
        [HS.PApp () (unqualSym $ toCamel funName) $
          map (\Field{..} -> pvar ("__field__" <> fieldName)) funArgs]
        (HS.UnGuardedRhs () $ stringLit funName)
        Nothing
      ]

    noParentBody False = []

    noParentBody True = [ HS.FunBind ()
      [ HS.Match () (textToName "reqName'")
        [ HS.PWildCard () ]
        (HS.UnGuardedRhs () (stringLit "unknown function"))
        Nothing
      ]]

-- | Generates a function that parses the input message as appropriate
genReqParser :: HS Service -> [HS.Decl ()]
genReqParser s@Service{..} =
  [ HS.TypeSig () [textToName "reqParser'"] $
    HS.TyForall () Nothing
#if MIN_VERSION_haskell_src_exts(1,22,0)
      (Just $ HS.CxSingle () $ HS.TypeA () (HS.TyApp () (HS.TyCon () (qualSym "Thrift" "Protocol"))
        (tvar "p" ))) $
#else
      (Just $ HS.CxSingle () $ HS.ClassA () (qualSym "Thrift" "Protocol")
        [ tvar "p" ]) $
#endif
    HS.TyFun () (qualType "Proxy" "Proxy" `appT` tvar "p") $
    HS.TyFun () (qualType "Text" "Text") $
    HS.TyApp () (qualType "Parser" "Parser") $ HS.TyParen () $
      qualType "Thrift" "Some" `appT` simpleType (commandTypeName s)
  ] ++
  map genFunction serviceFunctions ++
  genSuper serviceSuper
  where
    genSuper Nothing = [ HS.FunBind ()
      [ HS.Match () (textToName "reqParser'")
        [ HS.PWildCard (), pvar "funName"]
        (HS.UnGuardedRhs () (qvar "Prelude" "errorWithoutStackTrace" `app` HS.Paren ()
           (infixApp "++" (stringLit "unknown function call: ")
           (qvar "Text" "unpack" `app` var "funName"))))
        Nothing
      ]]
    genSuper (Just Super{..}) =
      let n = localName $ resolvedName $ fst supResolvedName in
      [ HS.FunBind ()
      [ HS.Match () (textToName "reqParser'")
        [ pvar "_proxy", pvar "funName" ]
        (HS.UnGuardedRhs () $ HS.Do ()
          [ HS.Generator () (HS.PApp () (qualSym "Thrift" THIS) [ pvar "x" ])
             (qvar (toCamel n) "reqParser'" `app`
              var "_proxy" `app`
              var "funName")
          , HS.Qualifier ()
             (qvar "Prelude" "return" `app` HS.Paren ()
               (qcon "Thrift" THIS `app`
                HS.Paren () (
                  con ("Super" <> n) `app`
                  var "x"
                )))
          ])
        Nothing
      ]]

    genFunction Function{..} = HS.FunBind ()
      [ HS.Match () (textToName "reqParser'")
        [ pvar "_proxy", stringP funName ]
        (genFieldParser (map (qualifyField "Types") funArgs)
         (toCamel funName)
         (app (qcon "Thrift" THIS)))
        Nothing
      ]

-- | Generates a function that builds the appropriate response type
genRespWriter :: HS Service -> [HS.Decl ()]
genRespWriter s@Service{..} =
  [ HS.TypeSig () [textToName "respWriter'"] $
    HS.TyForall () Nothing
#if MIN_VERSION_haskell_src_exts(1,22,0)
      (Just $ HS.CxSingle () $ HS.TypeA () (HS.TyApp () (HS.TyCon () (qualSym "Thrift" "Protocol"))
        (tvar "p" ))) $
#else
      (Just $ HS.CxSingle () $ HS.ClassA () (qualSym "Thrift" "Protocol")
        [ tvar "p" ]) $
#endif
    HS.TyFun () (qualType "Proxy" "Proxy" `appT` tvar "p") $
    HS.TyFun () (qualType "Int" "Int32") $
    HS.TyFun () (simpleType (commandTypeName s) `appT` tvar "a") $
    HS.TyFun ()
    (qualType "Prelude" "Either" `appT`
     qualType "Exception" "SomeException" `appT`
     tvar "a") $
    HS.TyTuple () Boxed
      [ qualType "Builder" "Builder"
      , qualType "Prelude" "Maybe" `appT`
          HS.TyTuple() Boxed
            [ qualType "Exception" "SomeException"
            , qualType "Thrift" "Blame"
            ]
      ]
  ] ++
  map genFunction serviceFunctions ++
  genSuper serviceSuper (null serviceFunctions)
  where
    genSuper Nothing True =
      [ HS.FunBind ()
        [ HS.Match () (textToName "respWriter'")
          [ HS.PWildCard () ]
          (HS.UnGuardedRhs () (qvar "Prelude" "errorWithoutStackTrace" `app` HS.Paren ()
            (stringLit "unknown function")))
          Nothing
        ]
      ]
    genSuper Nothing False = []
    genSuper (Just Super{..}) _ =
      let n = localName $ resolvedName $ fst supResolvedName
      in
        [ HS.FunBind ()
          [ HS.Match () (textToName "respWriter'")
            [ pvar "_proxy"
            , pvar "_seqNum"
            , HS.PParen () $ HS.PApp ()
              (HS.UnQual () $ textToName $ "Super" <> n) [pvar "_x"]
            , pvar "_r" ]
            (HS.UnGuardedRhs () $
             qvar n "respWriter'" `app`
             var "_proxy" `app`
             var "_seqNum" `app`
             var "_x" `app`
             var "_r"
            )
            Nothing
          ]]

    genFunction :: HS Function -> Decl ()
    genFunction Function{..} = HS.FunBind ()
      [ HS.Match () (textToName "respWriter'")
        [ pvar "_proxy"
        , pvar "_seqNum"
        , HS.PRec () (unqualSym $ toCamel funName) []
        , pvar "_r" ]
        (HS.UnGuardedRhs () $
          Tuple () Boxed
         [ infixApp "<>"
            (infixApp "<>"
              (protocolFun "genMsgBegin" `app`
              stringLit funName `app`
              var "_msgType" `app`
              var "_seqNum")
              (var "_msgBody"))
            (protocolFun "genMsgEnd")
          , var "_msgException" ])
        (Just $ BDecls ()
         [ PatBind () (PTuple () Boxed
            [ pvar "_msgType"
            , pvar "_msgBody"
            , pvar "_msgException" ])
           (UnGuardedRhs () $ Case () (var "_r")
            [ Alt () (PApp () (qualSym "Prelude" "Left") [ pvar "_ex" ])
              (GuardedRhss () $
               [ GuardedRhs ()
                 [ Generator ()
                   (PApp () (qualSym "Prelude" "Just")
                    [ PAsPat () (textToName "_e") $
                      PRec () (qualSym "Thrift" "ApplicationException") []
                    ]) $
                   qvar "Exception" "fromException" `app` var "_ex"
                 ] $
                 genRespTup genEXCEPTION
                  (protocolFun "buildStruct" `app` var "_e")
                  (Just (var "_ex", "ServerError"))
               ] ++
               map genExceptionCase funExceptions ++
               [ GuardedRhs () [ Qualifier () $ qvar "Prelude" "otherwise" ] $
                 Let ()
                  (BDecls ()
                    [ PatBind ()
                        (pvar "_e")
                        (UnGuardedRhs () $
                          qcon "Thrift" "ApplicationException" `app`
                          (qvar "Text" "pack" `app`
                            (qvar "Prelude" "show" `app` var "_ex")) `app`
                          qcon "Thrift"
                            "ApplicationExceptionType_InternalError")
                        Nothing ]) $
                  genRespTup genEXCEPTION
                    (protocolFun "buildStruct" `app` var "_e")
                    (Just
                      ( qvar "Exception" "toException" `app` var "_e"
                      , "ServerError" ))
               ])
              Nothing
            , Alt () (PApp () (qualSym "Prelude" "Right") [ pvar "_result" ])
              (UnGuardedRhs () $ genRespTup genREPLY
                (protocolFun "genStruct" `app`
                  HS.List () (genResp funResolvedType))
                Nothing)
              Nothing
            ])
           Nothing
         ])
      ]
      where
        genExceptionCase :: HS (Field 'Throws) -> GuardedRhs ()
        genExceptionCase Field{..} =
          case fieldTag of
            THROWS_RESOLVED -> GuardedRhs ()
              [ Generator ()
                (PApp () (qualSym "Prelude" "Just")
                 [ PAsPat () (textToName "_e") $
                   PRec () (genConstructor (Just "Types") fieldResolvedType) []
                 ])
                 (qvar "Exception" "fromException" `app` var "_ex")
              ] $
              genRespTup genREPLY
                (protocolFun "genStruct" `app`
                  HS.List ()
                  [ genFieldBase fieldResolvedType fieldName fieldId
                     (intLit (0 :: Int))
                   (  var "_e")
                  ])
                (Just (var "_ex", "ClientError"))

        genRespTup int body exc = Tuple () Boxed
          [ intLit int
          , body
          , case exc of
              Just (e,blame) -> qcon "Prelude" "Just" `app`
                Tuple () Boxed [ e, qcon "Thrift" blame ]
              Nothing -> qcon "Prelude" "Nothing"
          ]

        genResp Nothing = []
        genResp (Just (This t)) =
          [ genFieldBase t "" 0 (intLit (0 :: Int)) (var "_result") ]

genOneWays :: HS Service -> [HS.Decl ()]
genOneWays Service{..} =
  [ HS.TypeSig () [textToName "onewayFunctions'"] $
    HS.TyList () (qualType "Text" "Text")
  , HS.FunBind ()
    [ HS.Match () (textToName "onewayFunctions'") []
      (HS.UnGuardedRhs () $ genList $ (fst . supResolvedName) <$> serviceSuper)
      Nothing
    ]
  ]
  where
    genList Nothing = onewayFns
    genList (Just Name{..}) =
      infixApp "++" onewayFns (qvar (localName resolvedName) "onewayFunctions'")

    onewayFns = HS.List () $
      map (stringLit . funName) $ filter funIsOneWay serviceFunctions

commandTypeName :: HS Service -> Text
commandTypeName Service{..} = serviceResolvedName <> "Command"
