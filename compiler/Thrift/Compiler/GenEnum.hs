-- Copyright (c) Facebook, Inc. and its affiliates.

module Thrift.Compiler.GenEnum
  ( genEnumImports
  , genEnumDecl
  ) where

import Prelude hiding (Enum)
import Control.Monad
import Data.List
import Data.Text (Text)
import Language.Haskell.Exts.Syntax hiding (Type, Decl)
import qualified Data.Set as Set
import qualified Language.Haskell.Exts.Syntax as HS
import TextShow

import Thrift.Compiler.GenConst
import Thrift.Compiler.GenTypedef
import Thrift.Compiler.GenUtils
import Thrift.Compiler.Plugins.Haskell
import Thrift.Compiler.Types

-- Data Type Declaration -------------------------------------------------------

genEnumImports :: Set.Set Import
genEnumImports = Set.fromList
  [ QImport "Prelude" "Prelude"
  , QImport "Control.Exception" "Exception"
  , QImport "Control.DeepSeq" "DeepSeq"
  , QImport "Data.Aeson" "Aeson"
  , QImport "Data.Default" "Default"
  , QImport "Data.Function" "Function"
  , QImport "Data.Hashable" "Hashable"
  , QImport "Data.Int" "Int"
  , SymImport "Prelude" [ ".", "++", ">", "==" ]
  ]

genEnumDecl :: HS Enum -> [HS.Decl ()]
genEnumDecl Enum{ enumIsPseudo=True,..} =
  genTypedefDecl typedef ++
  concatMap genConstDecl consts
  where
    typedef = Typedef
      { tdName = enumName
      , tdResolvedName = enumResolvedName
      , tdTag = IsNewtype
      , tdType = AnnotatedType I32 Nothing (Arity0Loc nlc)
      , tdResolvedType = I32
      , tdLoc = TypedefLoc nlc nlc
      , tdAnns = Nothing
      , tdSAnns = []
      }
    consts = flip map enumConstants $ \EnumValue{..} -> Const
      { constName = evName
      , constResolvedName = evResolvedName
      , constType = AnnotatedType (TNamed enumName) Nothing (Arity0Loc nlc)
      , constResolvedType =
        TNewtype (mkName enumName enumResolvedName) I32 noLoc
      , constVal =
        UntypedConst nlc $ IntConst (fromIntegral evValue) (showt evValue)
      , constResolvedVal = Literal $ New $ fromIntegral evValue
      , constLoc = ConstLoc nlc nlc nlc NoSep
      , constSAnns = []
      }
genEnumDecl Enum{..} =
  -- Enum Declaration
  [ DataDecl () (DataType ()) Nothing
    (DHead () $ textToName enumResolvedName)
    -- We generate them in sorted order so that we can derive Bounded correctly
    ((genConstr <$> sortOn evValue enumConstants) ++ [genUnknownConstr | not enumNoUnknown])
    -- Deriving
    (if null enumConstants
     then mzero
     else
       pure $ deriving_ $ map (IRule () Nothing Nothing . IHCon ()) $
       [ qualSym "Prelude" "Eq"
       , qualSym "Prelude" "Show"
       ] ++ [ qualSym "Prelude" "Ord" | canDeriveOrd ])
  ] ++
  -- Instances
  if null enumConstants
  then
    map (genEmptyInstance enumResolvedName)
    -- Using the symbol (==) in the AST is technically wrong, but it
    -- generates correct pretty-printed code and allows us to reuse more code
    [ ("Prelude", "Eq", [ "(==)" ])
    , ("Prelude", "Show", [ "show" ])
    , ("Prelude", "Ord", [ "compare" ])
    , ("Aeson", "ToJSON", [ "toJSON" ])
    , ("Default", "Default", [ "def" ])
    , ("Hashable", "Hashable", [ "hashWithSalt" ])
    , ("DeepSeq", "NFData", [ "rnf" ])
    , ("Thrift", "ThriftEnum",
        [ "toThriftEnum", "fromThriftEnum", "allThriftEnumValues", "toThriftEnumEither" ]
      )
    ]
  else
    [ genToJSON enumResolvedName
    , genNFData enumResolvedName
    , genDefault enumResolvedName enumConstants
    , genHashable enumResolvedName
    , genThriftEnumInst enumResolvedName enumConstants enumNoUnknown
    ] ++
    [genOrd enumResolvedName | not canDeriveOrd]
  where
    genConstr :: HS EnumValue -> QualConDecl ()
    genConstr EnumValue{..} =
      QualConDecl () Nothing Nothing
                      (ConDecl () (textToName evResolvedName) [])
    -- If the stars align we can derive the Enum instance
    -- This requires the Enum to contain exactly the values [0 .. n-1]
    canDeriveOrd = and $ zipWith (==) [0..] $ sort $ map evValue enumConstants

    -- Use 2 underscores to avoid name collisions.
    genUnknownConstr :: QualConDecl ()
    genUnknownConstr =
      QualConDecl
        ()
        Nothing
        Nothing
        (ConDecl () (textToName $ enumResolvedName <> "__UNKNOWN") [genType (TSpecial HsInt)])

-- Ord Instance ----------------------------------------------------------------

genOrd :: Text -> HS.Decl ()
genOrd name =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym "Prelude" "Ord")
       (TyCon () $ unqualSym name))
    (Just $ map (InsDecl ())
     [ FunBind ()
       [ Match () (textToName "compare") []
         (UnGuardedRhs () $
          qvar "Function" "on" `app`
          qvar "Prelude" "compare" `app`
          qvar "Thrift" "fromThriftEnum")
         Nothing
       ]
     ])

-- Aeson Instances -------------------------------------------------------------

genToJSON :: Text -> HS.Decl ()
genToJSON name =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym "Aeson" "ToJSON")
       (TyCon () $ unqualSym name))
    (Just $ map (InsDecl ())
     [ FunBind ()
       [ Match () (textToName "toJSON") []
         (UnGuardedRhs () $
          qvar "Aeson" "toJSON" `compose` qvar "Thrift" "fromThriftEnum")
         Nothing
       ]
     ])

 -- Generate NFData Instance ---------------------------------------------------

genNFData :: Text -> HS.Decl ()
genNFData name =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym "DeepSeq" "NFData")
       (TyCon () $ unqualSym name))
    (Just $ map (InsDecl ())
     [ FunBind ()
       [ Match () (textToName "rnf")
         [ PApp () (unqualSym arg) [] ]
         (UnGuardedRhs () $
          qvar "Prelude" "seq" `app` var arg `app` unit)
         Nothing
       ]
     ])
    where
      arg = "__" <> name
      unit = Con () (Special () (UnitCon ()))

 -- Generate Default Instance --------------------------------------------------

genDefault :: Text -> [HS EnumValue] -> HS.Decl ()
genDefault name consts =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym "Default" "Default")
       (TyCon () $ unqualSym name))
    (Just $ map (InsDecl ())
     [ FunBind ()
       [ Match () (textToName "def") []
         (UnGuardedRhs () $
          case consts of
            EnumValue{..} : _ -> con evResolvedName
            [] ->
              qvar "Exception" "throw" `app`
              (qvar "Thrift" "ProtocolException" `app`
               stringLit ("def: enum " <> name <> "has no constructors")))
         Nothing
       ]
     ])

 -- Generate Hashable Instance -------------------------------------------------

genHashable :: Text -> HS.Decl ()
genHashable name =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym "Hashable" "Hashable")
       (TyCon () $ unqualSym name))
    (Just
     [ InsDecl () $ FunBind ()
       [ Match () (textToName "hashWithSalt") [ pvar "_salt", pvar "_val" ]
         (UnGuardedRhs () $
          qvar "Hashable" "hashWithSalt" `app` var "_salt" `app`
          (qvar "Thrift" "fromThriftEnum" `app` var "_val"))
         Nothing
       ]
     ])

-- Generate Empty Instance -----------------------------------------------------

genEmptyInstance :: Text -> (Text, Text, [Text]) -> HS.Decl ()
genEmptyInstance name (mname, className, methods) =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
     IHApp ()
       (IHCon () $ qualSym mname className)
       (TyCon () $ unqualSym name))
    (Just $ map
     (\method ->
       InsDecl () $ FunBind ()
       [ Match () (textToName method) []
         (UnGuardedRhs () $
          qvar "Exception" "throw" `app`
          (qvar "Thrift" "ProtocolException" `app`
           stringLit
           (mconcat
            [ method, ": Thrift enum '", name, "' is uninhabited"])))
         Nothing
       ])
     methods)

-- Thrift Enum Instance --------------------------------------------------------

genThriftEnumInst :: Text -> [HS EnumValue] -> Bool -> HS.Decl ()
genThriftEnumInst ename consts enumNoUnknown =
  InstDecl () Nothing
    (IRule () Nothing Nothing
      (IHApp ()
       (IHCon () (qualSym "Thrift" "ThriftEnum"))
       (TyCon () (unqualSym ename))))
    (Just $ map (InsDecl () . FunBind ())
       [ map genToEnumMatch consts
          ++ [ if enumNoUnknown
               then genToEnumCatchAll
               else genToEnumUnknown
             ]
       , map genFromEnumMatch consts
          ++ [ if enumNoUnknown
               then genFromEnumCatchAll
               else genFromEnumUnknown
             ]
       , genAllEnumValues consts
       , map genToEnumEitherMatch consts ++ [genToEnumEitherUnknown]
       ]
    )
  where
    genToEnumMatch :: HS EnumValue -> Match ()
    genToEnumMatch EnumValue{..} =
      Match ()
        (textToName "toThriftEnum")
        [ PLit ()
          (if evValue < 0 then Negative () else Signless ())
          (Int () (abs $ fromIntegral evValue) (show evValue))
        ]
        (UnGuardedRhs () $ Con () $ unqualSym evResolvedName)
        Nothing
    genToEnumCatchAll =
      Match ()
        (textToName "toThriftEnum")
        [ pvar "_val" ]
        (UnGuardedRhs () $
          qvar "Exception" "throw" `app`
          (qvar "Thrift" "ProtocolException" `app`
          infixApp "++"
          (stringLit $ "toThriftEnum: not a valid identifier for enum " <>
                       ename <> ": ")
          (qvar "Prelude" "show" `app` var "_val")))
        Nothing
    genToEnumUnknown =
      Match ()
        (textToName "toThriftEnum")
        [ pvar "val" ]
        (UnGuardedRhs () $ var (ename <> "__UNKNOWN") `app` var "val")
        Nothing
    genFromEnumMatch :: HS EnumValue -> Match ()
    genFromEnumMatch EnumValue{..} =
      Match ()
        (textToName "fromThriftEnum")
        [PApp () (unqualSym evResolvedName) []]
        (UnGuardedRhs () $ intLit evValue)
        Nothing
    genFromEnumCatchAll =
      Match ()
        (textToName "fromThriftEnum")
        [ pvar "_val" ]
        (UnGuardedRhs () $
          qvar "Exception" "throw" `app`
          (qvar "Thrift" "ProtocolException" `app`
          infixApp "++"
          (stringLit $ "fromThriftEnum: not a valid identifier for enum " <>
                       ename <> ": ")
          (qvar "Prelude" "show" `app` var "_val")))
        Nothing
    genFromEnumUnknown =
      Match ()
        (textToName "fromThriftEnum")
        [PApp () (unqualSym (ename <> "__UNKNOWN")) [pvar "val"]]
        (UnGuardedRhs () $ var "val")
        Nothing
    genToEnumEitherMatch :: HS EnumValue -> Match ()
    genToEnumEitherMatch EnumValue{..} =
      Match ()
        (textToName "toThriftEnumEither")
        [ PLit ()
          (if evValue < 0 then Negative () else Signless ())
          (Int () (abs $ fromIntegral evValue) (show evValue))
        ]
        (UnGuardedRhs () $ qvar "Prelude" "Right" `app` var evResolvedName)
        Nothing
    genToEnumEitherUnknown =
      Match ()
        (textToName "toThriftEnumEither")
        [ pvar "val" ]
        (UnGuardedRhs () $ qvar "Prelude" "Left" `app` infixApp "++"
          (stringLit $ "toThriftEnumEither: not a valid identifier for enum " <>
                     ename <> ": ")
          (qvar "Prelude" "show" `app` var "val"))
        Nothing

genAllEnumValues :: [HS EnumValue] -> [Match ()]
genAllEnumValues cs =
  [ Match
      ()
      (textToName "allThriftEnumValues")
      []
      (UnGuardedRhs () $ listE (genEnumExp <$> sortOn evValue cs))
      Nothing
  ]
  where
    genEnumExp :: HS EnumValue -> Exp ()
    genEnumExp EnumValue{..} = Con () $ unqualSym evResolvedName
