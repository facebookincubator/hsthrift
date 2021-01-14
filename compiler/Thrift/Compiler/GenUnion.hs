-- Copyright (c) Facebook, Inc. and its affiliates.

module Thrift.Compiler.GenUnion
  ( genUnionDecl
  , genUnionImports
  ) where

import Prelude hiding (exp)
import Data.Maybe
import Data.Text (Text)
import Language.Haskell.Exts.Syntax hiding
  (Name, Type, Annotation, Decl)
import qualified Data.Set as Set
import qualified Language.Haskell.Exts.Syntax as HS

import Thrift.Compiler.GenStruct
import Thrift.Compiler.GenUtils
import Thrift.Compiler.Plugins.Haskell
import Thrift.Compiler.Types

-- Generate Datatype -----------------------------------------------------------

genUnionImports :: HS Union -> Set.Set Import
genUnionImports Union{..} =
  foldr (Set.union . getImports) baseImports unionAlts
  where
    getImports :: HS UnionAlt -> Set.Set Import
    getImports UnionAlt{..} = typeToImport altResolvedType
    baseImports = Set.fromList
                  [ QImport "Prelude" "Prelude"
                  , QImport "Control.DeepSeq" "DeepSeq"
                  , QImport "Control.Exception" "Exception"
                  , QImport "Data.Aeson" "Aeson"
                  , QImport "Data.Aeson.Types" "Aeson"
                  , QImport "Data.Default" "Default"
                  , QImport "Data.Hashable" "Hashable"
                  , QImport "Data.HashMap.Strict" "HashMap"
                  , QImport "Data.List" "List"
                  , QImport "Data.Ord" "Ord"
                  , SymImport "Prelude" [ ".", "<$>", "<*>", ">>=", "==", "++" ]
                  , SymImport "Data.Aeson" [ ".:", ".=" ]
                  , SymImport "Data.Monoid" [ "<>" ]
                  ]

genUnionDecl :: HS Union -> [HS.Decl ()]
genUnionDecl union@Union{..} =
  -- Union Declaration
  [ DataDecl () (DataType ()) Nothing
    (DHead () $ textToName unionResolvedName)
    -- Data Constructors
    (map genAltDecl unionAlts ++
     case unionHasEmpty of
       NonEmpty -> []
       HasEmpty ->
         [ QualConDecl () Nothing Nothing $
           ConDecl () (textToName unionEmptyName) []
         ])

    -- Deriving
    (pure $ deriving_ $ map (IRule () Nothing Nothing . IHCon ()) $
           [ qualSym "Prelude" "Eq"
           , qualSym "Prelude" "Show"
           ] ++
           [ qualSym "Prelude" "Ord" | deriveOrd ])
  -- Aeson Instances
  , genToJSONInst union
  -- ThriftStruct Instance
  , genThriftStruct union
  -- Other Instances
  , genNFData union
  , genDefault union
  , genHashable union
  ] ++
  [ genOrd union | not deriveOrd ]
  where
    deriveOrd =
      all (\UnionAlt{..} -> isNothing $ mkOrd altResolvedType) unionAlts

genAltDecl :: HS UnionAlt -> QualConDecl ()
genAltDecl UnionAlt{..} =
  QualConDecl () Nothing Nothing $
  ConDecl () (textToName altResolvedName) [ genType altResolvedType ]

-- Generate Aeson Instances ----------------------------------------------------

genToJSONInst :: HS Union -> HS.Decl ()
genToJSONInst Union{..} =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym "Aeson" "ToJSON")
       (TyCon () $ unqualSym unionResolvedName))
    (Just [ InsDecl () $ FunBind () $
            map genToJSONAlt unionAlts ++
            case unionHasEmpty of
              NonEmpty -> []
              HasEmpty -> [ genToJSONEmpty unionEmptyName ]
          ])

genToJSONAlt :: HS UnionAlt -> Match ()
genToJSONAlt UnionAlt{..} =
  Match () (textToName "toJSON")
  [ PApp () (unqualSym altResolvedName) [ pvar arg ] ]
  (UnGuardedRhs () $
     qvar "Aeson" "object" `app`
     HS.List ()
     [ infixApp ".=" (stringLit altName) $
       case fixToJSONValue altResolvedType of
         Nothing -> var arg
         Just f  -> f `app` var arg
     ])
  Nothing
  where
    arg = "__" <> altName

genToJSONEmpty :: Text -> Match ()
genToJSONEmpty uname =
  Match () (textToName "toJSON")
  [ PApp () (unqualSym uname) [] ]
  (UnGuardedRhs () $
     qvar "Aeson" "object" `app` HS.List () [])
  Nothing

-- Generate ThriftStruct Instance ----------------------------------------------

genThriftStruct :: HS Union -> HS.Decl ()
genThriftStruct union@Union{..} =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym "Thrift" "ThriftStruct")
       (TyCon () $ unqualSym unionResolvedName))
    (Just $ map (InsDecl ())
     [ genBuilder union
     , genParser union
     ])

genBuilder :: HS Union -> HS.Decl ()
genBuilder Union{..} =
  FunBind () $
  (flip map unionAlts $ \UnionAlt{..} ->
    let arg = "__" <> altName in
    mkMatch (PApp () (unqualSym altResolvedName) [ pvar arg ])
    [ genFieldBase altResolvedType altName altId lastId (var arg) ]) ++
  case unionHasEmpty of
    NonEmpty -> []
    HasEmpty -> [ mkMatch (PApp () (unqualSym unionEmptyName) []) [] ]
  where
    lastId = intLit (0 :: Int)
    mkMatch pat fields =
      Match () (textToName "buildStruct")
      [ pvar "_proxy", pat ]
      (UnGuardedRhs () $
       protocolFun "genStruct" `app`
       HS.List () fields)
      Nothing

genParser :: HS Union -> HS.Decl ()
genParser Union{..} =
  FunBind ()
  [ Match ()
    (textToName "parseStruct")
    [ pvar "_proxy" ]
    (UnGuardedRhs () $ Do ()
     [ Generator () (pvar "_fieldBegin") $
       protocolFun "parseFieldBegin" `app` lastId `app` var "_idMap"
     , Qualifier () $
       Case () (var "_fieldBegin")
       [ Alt ()
         (PApp () (qualSym "Thrift" "FieldBegin")
          [ pvar "_type", pvar "_id", pvar "_bool" ])
         (UnGuardedRhs () $ Do ()
          [ Qualifier () $ Case () (var "_id") $
            map genParseValue unionAlts ++
            [ Alt () (PWildCard ())
              (UnGuardedRhs () $ case unionHasEmpty of
                NonEmpty ->
                  qvar "Prelude" "fail" `app`
                  (infixApp "++"
                   (stringLit
                    ("unrecognized alternative for union '" <>
                     unionName <>
                     "': "))
                   (qvar "Prelude" "show" `app` var "_id"))
                HasEmpty -> Do ()
                  [ Qualifier () $
                      protocolFun "parseSkip" `app`
                      var "_type" `app`
                      qcon "Prelude" "Nothing"
                  , Qualifier () $ protocolFun "parseStop"
                  , Qualifier () $
                      qvar "Prelude" "return" `app` con unionEmptyName
                  ])
              Nothing
            ]
          ])
         Nothing
       , Alt () (PApp () (qualSym "Thrift" "FieldEnd") [])
         (UnGuardedRhs () $
          case unionHasEmpty of
            NonEmpty -> qvar "Prelude" "fail" `app`
                        stringLit ("union '" <> unionName <> "' is empty")
            HasEmpty -> qvar "Prelude" "return" `app` con unionEmptyName)
         Nothing
       ]
     ])
    (Just $ BDecls ()
     [ PatBind () (pvar "_idMap")
       (UnGuardedRhs () $
        qvar "HashMap" "fromList" `app`
        HS.List ()
        (map (\UnionAlt{..} ->
         Tuple () Boxed [ stringLit altName, intLit altId ])
         unionAlts))
       Nothing
     ])
  ]
  where
    lastId = intLit (0 :: Int)

genParseValue :: HS UnionAlt -> Alt ()
genParseValue UnionAlt{..} =
  Alt ()
  (PLit ()
   (if altId < 0 then Negative () else Signless ())
   (Int () (abs $ fromIntegral altId) (show altId)))
  (GuardedRhss ()
   -- check that the parsed type is correct
   [ GuardedRhs ()
     [ Qualifier () $
       infixApp "==" (var "_type") (genThriftType altResolvedType)
     ] $ Do ()
     [ Generator () (pvar "_val") $ genParseType P_FieldMode altResolvedType
     , Qualifier () $ protocolFun "parseStop"
     , Qualifier () $
       qvar "Prelude" "return" `app`
       (con altResolvedName `app` var "_val")
     ]
   ])
  Nothing

-- Generate NFData Instance ----------------------------------------------------

genNFData :: HS Union -> HS.Decl ()
genNFData Union{..} =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym "DeepSeq" "NFData")
       (TyCon () $ unqualSym unionResolvedName))
    (Just
     [ InsDecl () $ FunBind () $
       (flip map unionAlts $ \UnionAlt{..} ->
         Match () (textToName "rnf")
         [ PApp () (unqualSym altResolvedName) [ pvar ("__" <> altName) ] ]
         (UnGuardedRhs () $ qvar "DeepSeq" "rnf" `app` var ("__" <> altName))
         Nothing) ++
       case unionHasEmpty of
         NonEmpty -> []
         HasEmpty ->
           [ Match () (textToName "rnf")
             [ PApp () (unqualSym unionEmptyName) [] ]
             (UnGuardedRhs () $ unit_con ())
             Nothing
           ]
     ])

-- Generate Default Instance ---------------------------------------------------

genDefault :: HS Union -> HS.Decl ()
genDefault Union{..} =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym "Default" "Default")
       (TyCon () $ unqualSym unionResolvedName))
    (Just
     [ InsDecl () $ FunBind ()
       [ Match () (textToName "def") []
         (UnGuardedRhs () $
          case (unionHasEmpty, unionAlts) of
            (HasEmpty, _) -> con unionEmptyName
            (NonEmpty, UnionAlt{..} : _) ->
              con altResolvedName `app` typeToDefault altResolvedType
            (NonEmpty, []) ->
              qvar "Exception" "throw" `app`
              (qcon "Thrift" "ProtocolException" `app`
               stringLit
               ("def: no default value for empty union '" <> unionResolvedName
                <> "'")))
         Nothing
       ]
     ])

-- Generate Default Instance ---------------------------------------------------

genHashable :: HS Union -> HS.Decl ()
genHashable Union{..} =
  InstDecl () Nothing
  (IRule () Nothing Nothing $
   IHApp ()
   (IHCon () $ qualSym "Hashable" "Hashable")
   (TyCon () $ unqualSym unionResolvedName))
  (Just
   [ InsDecl () $ FunBind () $
     map genHashWithSalt unionAlts ++
     case unionHasEmpty of
       NonEmpty -> []
       HasEmpty -> [ genHashWithSaltEmpty unionEmptyName ]
   ])

genHashWithSalt :: HS UnionAlt -> Match ()
genHashWithSalt UnionAlt{..} =
  Match () (textToName "hashWithSalt")
  [ pvar "__salt"
  , PApp () (unqualSym altResolvedName)
    [ pvar $ "_" <> altName ]
  ]
  (UnGuardedRhs () $
   qvar "Hashable" "hashWithSalt" `app`
   var "__salt" `app`
   (qvar "Hashable" "hashWithSalt" `app`
    intLit altId `app`
    transformValue mkHashable Default altResolvedType (var $ "_" <> altName)))
  Nothing

genHashWithSaltEmpty :: Text -> Match ()
genHashWithSaltEmpty uname =
  Match () (textToName "hashWithSalt")
  [ pvar "__salt"
  , PApp () (unqualSym uname) []
  ]
  (UnGuardedRhs () $
   qvar "Hashable" "hashWithSalt" `app`
   var "__salt" `app`
   ExpTypeSig () (intLit (0 ::Int)) (qualType "Prelude" "Int"))
  Nothing

-- Generate Default Instance ---------------------------------------------------

genOrd :: HS Union -> HS.Decl ()
genOrd Union{..} =
  InstDecl () Nothing
  (IRule () Nothing Nothing $
   IHApp ()
   (IHCon () $ qualSym "Ord" "Ord")
   (TyCon () $ unqualSym unionName))
  (Just
   [ InsDecl () $ FunBind () $
     concatMap genCompare unionAlts ++
     case unionHasEmpty of
       NonEmpty -> []
       HasEmpty -> genCompareEmpty unionEmptyName
   ])

genCompare :: HS UnionAlt -> [Match ()]
genCompare UnionAlt{..} =
  [ Match () (textToName "compare")
    [ PApp () (unqualSym altResolvedName) [pvar "__a"]
    , PApp () (unqualSym altResolvedName) [pvar "__b"]
    ]
    (UnGuardedRhs () $
     qvar "Ord" "compare" `app`
     transform (var "__a") `app`
     transform (var "__b"))
    Nothing
  , Match () (textToName "compare")
    [ PApp () (unqualSym altResolvedName) [PWildCard ()]
    , PWildCard ()
    ]
    (UnGuardedRhs () $ qcon "Ord" "LT")
    Nothing
  ]
  where
    transform = transformValue mkOrd Default altResolvedType

genCompareEmpty :: Text -> [Match ()]
genCompareEmpty uname =
  [ Match () (textToName "compare")
    [ PApp () (unqualSym uname) []
    , PApp () (unqualSym uname) []
    ]
    (UnGuardedRhs () $ qcon "Ord" "EQ")
    Nothing
  , Match () (textToName "compare")
    [ PApp () (unqualSym uname) []
    , PWildCard ()
    ]
    (UnGuardedRhs () $ qcon "Ord" "GT")
    Nothing
  ]
