-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}

module Thrift.Compiler.GenFunction
  ( genFunctionDecls
  , genFunctionImports
  ) where

#if __GLASGOW_HASKELL__ > 804
#define This Some
#endif

import Data.Maybe
import Data.Set (union)
import Data.Some
import Data.Text (Text)
import Language.Haskell.Exts.Syntax hiding (Type)
import qualified Data.Set as Set
import qualified Language.Haskell.Exts.Syntax as HS

import Thrift.Compiler.Types
import Thrift.Compiler.GenStruct
import Thrift.Compiler.GenUtils
import Thrift.Compiler.Plugins.Haskell
import Util.HSE

-- Generate Imports and Decls --------------------------------------------------

genFunctionImports :: HS Function -> Set.Set Import
genFunctionImports Function{..} =
  foldr (union . getImports)
  (foldr (union . getImports) baseImports funExceptions)
  funArgs `union`
  maybe Set.empty (`withSome` typeToImport) funResolvedType
  where
    getImports :: HS (Field u) -> Set.Set Import
    getImports Field{..} = typeToImport fieldResolvedType
    baseImports = Set.fromList
      [ QImport "Prelude" "Prelude"
      , QImport "Control.Arrow" "Arrow"
      , QImport "Control.Exception" "Exception"
      , QImport "Control.Concurrent" "Concurrent"
      , QImport "Control.Monad" "Monad"
      , QImport "Control.Monad.Trans.Class" "Trans"
      , QImport "Control.Monad.Trans.Reader" "Reader"
      , QImport "Data.ByteString.Builder" "ByteString"
      , QImport "Data.ByteString.Lazy" "LBS"
      , QImport "Data.Int" "Int"
      , QImport "Data.HashMap.Strict" "HashMap"
      , QImport "Data.List" "List"
      , QImport "Data.Proxy" "Proxy"
      , QImport "Thrift.Binary.Parser" "Parser"
      , QImport "Thrift.Protocol.ApplicationException.Types" "Thrift"
      , SymImport "Prelude" [ "==", "=<<", ">>=", "<$>", "." ]
      , SymImport "Data.Monoid" [ "<>" ]
      ]

genFunctionDecls
  :: HS Service -> HS Function -> [HS.Decl ()]
genFunctionDecls service func = concatMap ($ func)
  [ genFunctionThrift service
  , genFunctionIO service
  , genFunctionSend service
  , genFunctionRecv
  , genBuildCall
  , genParseResponse
  ]

-- Generate the Function Call In the Thrift Monad ------------------------------

genFunctionThrift
  :: HS Service -> HS Function -> [HS.Decl ()]
genFunctionThrift Service{..} Function{..} =
  -- Type Signature
  [ TypeSig () [ name ] $
    TyForall () Nothing (commonCtx serviceResolvedName) $
    genArgTypes funArgs
    (qualType "Thrift" "ThriftM" `appT`
     tvar "p" `appT`
     tvar "c" `appT`
     tvar "s" `appT`
     maybe (unit_tycon ()) (`withSome` genType) funResolvedType)
  -- Function Body
  , FunBind ()
    [ Match () name
      (map (pvar . ("__field__" <>) . fieldName) funArgs)
      (UnGuardedRhs () $ Do ()
       [ Generator ()
         (PApp () (qualSym "Thrift" "ThriftEnv")
          (map pvar [ "_proxy", "_channel", "_opts", "_counter" ])) $
         qvar "Reader" "ask"
       , Qualifier () $
         qvar "Trans" "lift" `app`
         genApplyArgs funArgs
         (var (funResolvedName <> "IO") `app`
          var "_proxy" `app`
          var "_channel" `app`
          var "_counter" `app`
          var "_opts")
       ])
      Nothing
    ]
  ]
  where
    name = textToName funResolvedName

-- Generate the Function Call in the IO Monad ----------------------------------

genFunctionIO
  :: HS Service -> HS Function -> [HS.Decl ()]
genFunctionIO Service{..} fun@Function{..} =
  -- Type Signature
  [ TypeSig () [ textToName (funResolvedName <> "IO") ] $
    TyForall () Nothing (commonCtx serviceResolvedName) $
    TyFun () (qualType "Proxy" "Proxy" `appT` tvar "p") $
    TyFun () (tvar "c" `appT` tvar "s") $
    TyFun () (qualType "Thrift" "Counter") $
    TyFun () (qualType "Thrift" "RpcOptions") $
    genArgTypes funArgs
    (qualType "Prelude" "IO" `appT`
     maybe (unit_tycon ()) (`withSome` genType) funResolvedType)
  -- Function Body
  , FunBind ()
    [ Match () (textToName $ funResolvedName <> "IO")
      (map pvar $
       [ "_proxy", "_channel", "_counter", "_opts" ] ++
       map (("__field__" <>) . fieldName) funArgs)
      (UnGuardedRhs () $ genFunctionIOBody fun)
      Nothing
    ]
  ]

genFunctionIOBody :: HS Function -> Exp ()
genFunctionIOBody fun@Function{..}
  | funIsOneWay = Do () $
    [ Generator () (pvar "_handle") $ qvar "Concurrent" "newEmptyMVar"
    , Qualifier () $
      genApplyArgs funArgs $
      var (sendName fun) `app`
      var "_proxy" `app`
      var "_channel" `app`
      var "_counter" `app`
      (qvar "Concurrent" "putMVar" `app` var "_handle") `app`
      var "_opts"
    , Qualifier () $ infixApp ">>="
      (qvar "Concurrent" "takeMVar" `app` var "_handle")
      (qvar "Prelude" "maybe" `app`
       (qvar "Prelude" "return" `app` unit_con ()) `app`
       (qvar "Exception" "throw"))
    ]
  | otherwise = Do () $
    [ Generator ()
      (PTuple () Boxed [ pvar "_handle", pvar "_sendCob", pvar "_recvCob" ]) $
      qvar "Thrift" "mkCallbacks" `app`
      (var (recvName fun) `app` var "_proxy")
    , Qualifier () $
      genApplyArgs funArgs $
      var (sendName fun) `app`
      var "_proxy" `app`
      var "_channel" `app`
      var "_counter" `app`
      var "_sendCob" `app`
      var "_recvCob" `app`
      var "_opts"
    , Qualifier () $ qvar "Thrift" "wait" `app` var "_handle"
    ]

-- Generate Send Call ----------------------------------------------------------

genFunctionSend
  :: HS Service -> HS Function -> [HS.Decl ()]
genFunctionSend Service{..} fun@Function{..} =
  -- Type Signature
  [ TypeSig () [ textToName (sendName fun) ] $
    TyForall () Nothing (commonCtx serviceResolvedName) $
    TyFun () (qualType "Proxy" "Proxy" `appT` tvar "p") $
    TyFun () (tvar "c" `appT` tvar "s") $
    TyFun () (qualType "Thrift" "Counter") $
    TyFun () (qualType "Thrift" "SendCallback") $
    (if funIsOneWay
     then id
     else TyFun () (qualType "Thrift" "RecvCallback")) $
    TyFun () (qualType "Thrift" "RpcOptions") $
    genArgTypes funArgs $
    qualType "Prelude" "IO" `appT` unit_tycon ()
  -- Function Body
  , FunBind ()
    [ Match () (textToName $ sendName fun)
      (map pvar $
       [ "_proxy", "_channel", "_counter", "_sendCob" ] ++
       (if funIsOneWay
        then []
        else [ "_recvCob" ]) ++
       "_rpcOpts" :
       map (("__field__" <>) . fieldName) funArgs)
      (UnGuardedRhs () $ Do ()
       [ Generator () (pvar "_seqNum") (var "_counter")
       , LetStmt () $ BDecls ()
         [ PatBind () (pvar "_callMsg")
           (UnGuardedRhs () $
            qvar "LBS" "toStrict" `app`
            (qvar "ByteString" "toLazyByteString" `app`
             genApplyArgs funArgs
             (var (buildName fun) `app`
              var "_proxy" `app`
              var "_seqNum")))
           Nothing
         ]
       , Qualifier () $
         if funIsOneWay
         then
           qvar "Thrift" "sendOnewayRequest" `app`
           var "_channel" `app`
           (qcon "Thrift" "Request" `app`
            var "_callMsg" `app`
            rpcAst) `app`
           var "_sendCob"
         else
           qvar "Thrift" "sendRequest" `app`
           var "_channel" `app`
           (qcon "Thrift" "Request" `app`
            var "_callMsg" `app`
            rpcAst) `app`
           var "_sendCob" `app`
           var "_recvCob"
       ])
      Nothing
    ]
  ]
  where
    rpcAst = App ()
      (App ()
        (Var () (Qual () (ModuleName () "Thrift") (Ident () "setRpcPriority")))
        (Var () (UnQual () (Ident () "_rpcOpts")))
      )
      (Var () (Qual () (ModuleName () "Thrift") (Ident () priorityString)))
    priorityString = show $ case funPriority of
      NPriorities -> NormalPriority
      _           -> funPriority

sendName :: HS Function -> Text
sendName Function{..} = "send_" <> funResolvedName

-- Generate Recv Call ----------------------------------------------------------

genFunctionRecv :: HS Function -> [HS.Decl ()]
genFunctionRecv fun@Function{..}
  | funIsOneWay = []
  | otherwise =
  -- Type Signature
  [ TypeSig () [ textToName (recvName fun) ] $
    TyForall () Nothing
    (Just $ CxTuple ()
     [ classA () (qualSym "Thrift" "Protocol") [ tvar "p" ]
     ]) $
    TyFun () (qualType "Proxy" "Proxy" `appT` tvar "p") $
    TyFun () (qualType "Thrift" "Response") $
    qualType "Prelude" "Either" `appT`
    qualType "Exception" "SomeException" `appT`
    maybe (unit_tycon ()) (`withSome` genType) funResolvedType

  -- Function Body
  , FunBind ()
    [ Match () (textToName $ recvName fun)
      [ pvar "_proxy"
      , PApp () (qualSym "Thrift" "Response")
        [ pvar "_response", PWildCard () ]
      ]
      (UnGuardedRhs () $
       app (qvar "Monad" "join") $
       qvar "Arrow" "left" `app`
       (qvar "Exception" "SomeException" `compose`
        qcon "Thrift" "ProtocolException") `app`
       (qvar "Parser" "parse" `app`
        (var (parseName fun) `app` var "_proxy") `app`
        var "_response"))
       Nothing
    ]
  ]

recvName :: HS Function -> Text
recvName Function{..} = "recv_" <> funResolvedName

-- Generate the Function Call Message Builder ----------------------------------

genBuildCall :: HS Function -> [HS.Decl ()]
genBuildCall fun@Function{..} =
  -- Type Signature
  [ TypeSig () [ textToName (buildName fun) ] $
    TyForall () Nothing
    (Just $ CxSingle () $
     classA () (qualSym "Thrift" "Protocol") [ tvar "p" ]) $
    TyFun () (qualType "Proxy" "Proxy" `appT` tvar "p") $
    TyFun () (qualType "Int" "Int32") $
    genArgTypes funArgs (qualType "ByteString" "Builder")
  -- Function Body
  , FunBind ()
    [ Match () (textToName (buildName fun))
      (pvar "_proxy" :
       pvar "_seqNum" :
       map (pvar . ("__field__" <>) . fieldName) funArgs)
      (UnGuardedRhs () $
       infixApp "<>"
       (infixApp "<>"
        (protocolFun "genMsgBegin" `app`
         stringLit funName `app`
         intLit genCALL `app`
         var "_seqNum")
        (genBuildFields funArgs))
       (protocolFun "genMsgEnd"))
      Nothing
    ]
  ]

buildName :: HS Function -> Text
buildName Function{..} = "_build_" <> funResolvedName

-- Generate the Response Parser ------------------------------------------------

genParseResponse :: HS Function -> [HS.Decl ()]
genParseResponse func@Function{..}
  | funIsOneWay = []
  | otherwise =
  -- Type Signature
  [ TypeSig () [ textToName (parseName func) ] $
    TyForall () Nothing
    (Just $ CxSingle () $
     classA () (qualSym "Thrift" "Protocol") [ tvar "p" ]) $
    TyFun () (qualType "Proxy" "Proxy" `appT` tvar "p") $
    qualType "Parser" "Parser" `appT`
    (qualType "Prelude" "Either" `appT`
     qualType "Exception" "SomeException" `appT`
     case funResolvedType of
       Nothing -> unit_tycon ()
       Just (This ty) -> genType ty)
  -- Function Body
  , FunBind ()
    [ Match () (textToName (parseName func)) [ pvar "_proxy" ]
      (UnGuardedRhs () $ Do ()
       -- Parse the beginning of the message
       [ Generator ()
         (PApp () (qualSym "Thrift" "MsgBegin")
          [ pvar "_name", pvar "_msgTy", PWildCard () ]) $
         protocolFun "parseMsgBegin"
       , Generator () (pvar "_result") $
         Case () (var "_msgTy")
         -- Case 1: CALL
         [ Alt () (intP genCALL)
           (UnGuardedRhs () $
            qvar "Prelude" "fail" `app`
            stringLit (funName <> ": expected reply but got function call"))
           Nothing
         -- Case 2: REPLY
         , Alt () (intP genREPLY)
           (GuardedRhss ()
            [ GuardedRhs ()
              [ Qualifier () $ infixApp "==" (var "_name") (stringLit funName) ]
              (genParseReply func)
            , GuardedRhs () [ Qualifier () $ qvar "Prelude" "otherwise" ] $
              qvar "Prelude" "fail" `app`
              stringLit "reply function does not match"
            ])
           Nothing
         -- Case 3: EXCEPTION
         , Alt () (intP genEXCEPTION)
           (UnGuardedRhs () $
            qvar "Prelude" "fmap" `app`
            (qcon "Prelude" "Left" `compose`
             qcon "Exception" "SomeException") `app`
            ExpTypeSig ()
            (protocolFun "parseStruct")
            (qualType "Parser" "Parser" `appT`
             qualType "Thrift" "ApplicationException"))
           Nothing
         -- Case 4: ONEWAY
         , Alt () (intP genONEWAY)
           (UnGuardedRhs () $
            qvar "Prelude" "fail" `app`
            stringLit
            (funName <> ": expected reply but got oneway function call"))
           Nothing
         -- Catch All
         , Alt () (PWildCard ())
           (UnGuardedRhs () $
            qvar "Prelude" "fail" `app`
            stringLit (funName <> ": invalid message type"))
           Nothing
         ]
       , Qualifier () $ protocolFun "parseMsgEnd"
       , Qualifier () $ qvar "Prelude" "return" `app` var "_result"
       ])
      Nothing
    ]
  ]

parseName :: HS Function -> Text
parseName Function{..} = "_parse_" <> funResolvedName

-- This assumes that the response is basically a union, only one response is
-- possible.
genParseReply :: HS Function -> Exp ()
genParseReply Function{..} = Do ()
  [ LetStmt () $ BDecls ()
    [ PatBind () (pvar "_idMap")
      (UnGuardedRhs () $
       qvar "HashMap" "fromList" `app`
       HS.List ()
       (map (\Field{..} ->
        Tuple () Boxed [ stringLit fieldName, intLit fieldId ])
        fields))
      Nothing
    ]
  , Generator () (pvar "_fieldBegin") $
    protocolFun "parseFieldBegin" `app` intLit (0 :: Int)  `app` var "_idMap"
  , Qualifier () $
    Case () (var "_fieldBegin")
    [ Alt ()
      (PApp () (qualSym "Thrift" "FieldBegin")
       [ pvar "_type", pvar "_id", pvar "_bool" ])
      (UnGuardedRhs () $ Do ()
       [ Qualifier () $ Case () (var "_id") $
         maybeToList ((`withSome` genParseResult) <$> funResolvedType) ++
         map genParseException funExceptions ++
         [ Alt () (PWildCard ())
           (UnGuardedRhs () $
            qvar "Prelude" "fail" `app`
            (qvar "Prelude" "unwords" `app`
             HS.List ()
             [ stringLit "unrecognized exception, type:"
             , qvar "Prelude" "show" `app` var "_type"
             , stringLit "field id:"
             , qvar "Prelude" "show" `app` var "_id"
             ]))
           Nothing
         ]
       ])
      Nothing
    , Alt () (PApp () (qualSym "Thrift" "FieldEnd") [])
      (UnGuardedRhs () $
       case funResolvedType of
        Nothing ->
          qvar "Prelude" "return" `app`
          (qcon "Prelude" "Right" `app` unit_con ())
        Just{} -> qvar "Prelude" "fail" `app` stringLit "no response")
      Nothing
    ]
  ]
  where
    resultField :: HSType t -> HS (Field 'StructField)
    resultField ty = Field
      { fieldId     = 0
      , fieldName   = funName <> "_success"
      , fieldResolvedType   = ty
      , fieldRequiredness = Default
      , fieldTag = STRUCT_FIELD
      -- The following are placeholders since the fields aren't needed
      , fieldResolvedName = ""
      , fieldType = AnnotatedType I8 Nothing (Arity0Loc nlc)
      , fieldResolvedVal = Nothing
      , fieldVal = Nothing
      , fieldLaziness = Lazy
      , fieldLoc = FieldLoc nlc "0" nlc nlc Nothing NoSep
      , fieldAnns = Nothing
      , fieldSAnns = []
      }
    structify
      :: HS (Field 'Throws)
      -> HS (Field 'StructField)
    structify Field{ fieldRequiredness = Default, ..} =
      Field{ fieldTag = STRUCT_FIELD, fieldRequiredness = Default, .. }
    fields = maybe id (`withSome` ((:) . resultField)) funResolvedType $
             map structify funExceptions

genParseResult :: HSType t -> Alt ()
genParseResult ty =
  Alt () (intP 0)
  (GuardedRhss ()
   [ GuardedRhs ()
     [ Qualifier () $ infixApp "==" (var "_type") (genThriftType ty) ] $
     qvar "Prelude" "fmap" `app`
     qcon "Prelude" "Right" `app`
     genParseType P_FieldMode ty
   ])
  Nothing

genParseException :: HS (Field 'Throws) -> Alt ()
genParseException Field{..} =
  Alt () (intP $ fromIntegral fieldId)
  (GuardedRhss ()
   [ GuardedRhs ()
     [ Qualifier () $ infixApp "==" (var "_type")
       (genThriftType fieldResolvedType) ] $
     qvar "Prelude" "fmap" `app`
     (qvar "Prelude" "Left" `compose` qvar "Exception" "SomeException") `app`
     ExpTypeSig ()
     (genParseType P_FieldMode fieldResolvedType)
     (qualType "Parser" "Parser" `appT` genType fieldResolvedType)
   ])
  Nothing

-- Helpers ---------------------------------------------------------------------

commonCtx :: Text -> Maybe (HS.Context ())
commonCtx sname = Just $ CxTuple ()
  -- Protocol Constraint
  [ classA () (qualSym "Thrift" "Protocol") [ tvar "p" ]
  -- ClientChannel Constraint
  , classA () (qualSym "Thrift" "ClientChannel")  [ tvar "c" ]
  -- Service Subtyping Constraint
  , classA ()
    (Qual () (ModuleName () "Thrift") (Symbol () "<:"))
    [ tvar "s"
    , simpleType sname
    ]
  ]

genArgTypes :: [HS (Field 'Argument)] -> HS.Type () -> HS.Type ()
genArgTypes = flip $ foldr (\Field{..} -> TyFun () $ genType fieldResolvedType)

genApplyArgs :: [HS (Field 'Argument)] -> Exp () -> Exp ()
genApplyArgs =
  flip $ foldl (\f Field{..} -> f `app` var ("__field__" <> fieldName))
