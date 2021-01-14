-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
module Thrift.ExactPrint.Codemod
  ( roundTripWith
  , codemodConsts
  , lookupEnum
  ) where

import qualified Data.Map.Strict as Map
import Data.Some
import qualified Data.Text.Lazy as Text
import Data.Text (Text)

import Thrift.Compiler.Options
import Thrift.Compiler.Parser
import Thrift.Compiler.Pretty
import Thrift.Compiler.Typechecker
import qualified Thrift.Compiler.Typechecker.Monad as Typechecker
import Thrift.Compiler.Types
import Thrift.Compiler.Plugins.Linter

import Thrift.ExactPrint.Convert
import Thrift.ExactPrint.PrettyPrint
import Thrift.ExactPrint.Types

#if MIN_VERSION_dependent_sum(0,6,0)
#define This Some
#endif

-- | Round Trip a file, applying some tranformation to the typechecked AST
roundTripWith
  :: (Program Linter Offset -> Program Linter Offset)
  -> String
  -> String
roundTripWith f input = case runParser parseThrift "" input of
  Left e -> error e
  Right tf -> case typecheck (defaultOptions NoOpts) (mkModuleMap tf) of
    Left es -> error $ concatMap renderTypeError es
    Right (prog,_) -> Text.unpack $ exactPrint $ f $ computeOffsets prog

mkModuleMap :: ([Header Loc], [Parsed Decl]) -> ModuleMap
mkModuleMap (headers, decls) =
  Map.singleton "" ThriftFile
    { thriftName = ""
    , thriftPath = ""
    , thriftHeaders = headers
    , thriftDecls   = decls
    , thriftSplice = Nothing
    , thriftComments = []
    }

-- | Type aware codemod for constant values everywhere in the AST
codemodConsts
  :: forall l a. (forall t. Env l -> Type l t -> ConstVal a -> ConstVal a)
  -> Program l a
  -> Program l a
codemodConsts f p@Program{..} = p { progDecls = map codemodDecl progDecls }
  where
    codemodDecl (D_Struct s@Struct{..}) =
      D_Struct s { structMembers = map codemodField structMembers }
    codemodDecl (D_Const c@Const{..}) =
      D_Const c { constVal = codemodConst constResolvedType constVal }
    -- These have no constants
    codemodDecl d@D_Enum{} = d
    codemodDecl d@D_Typedef{} = d
    codemodDecl d@D_Union{} = d
    -- Services not supported yet
    codemodDecl d@D_Service{} = d

    codemodField :: Field u 'Resolved l a -> Field u 'Resolved l a
    codemodField fl@Field{..} =
      fl { fieldVal = codemodConst fieldResolvedType <$> fieldVal }

    codemodConst :: Type l t -> UntypedConst a -> UntypedConst a
    codemodConst ty c@UntypedConst{..} =
      c { ucConst = codemodConstVal ty ucConst }

    -- Recursively apply the refactoring function to everything
    codemodConstVal :: Type l t -> ConstVal a -> ConstVal a

    -- List Types
    codemodConstVal t@(TList u) c = codemodListVal t u c
    codemodConstVal t@(TSet u) c = codemodListVal t u c
    codemodConstVal t@(THashSet u) c = codemodListVal t u c

    -- Map Types
    codemodConstVal t@(TMap k v) c = codemodMapVal t k v c
    codemodConstVal t@(THashMap k v) c = codemodMapVal t k v c

    -- Aliases
    codemodConstVal t@(TTypedef _ u _loc) c = f progEnv t $ codemodConstVal u c
    codemodConstVal t@(TNewtype _ u _loc) c = f progEnv t $ codemodConstVal u c

    -- Structs
    codemodConstVal t@(TStruct name _loc) c
      | Just (This schema) <- lookupSchema name progEnv =
          f progEnv t $ codemodSchema schema c
    codemodConstVal t@(TException name _loc) c
      | Just (This schema) <- lookupSchema name progEnv =
          f progEnv t $ codemodSchema schema c
    codemodConstVal t@(TUnion name _loc) c
      | Just (This schema) <- lookupUnion name progEnv =
          f progEnv t $ codemodSchema schema c

    -- Everything Else
    codemodConstVal t c = f progEnv t c

    -- Apply the refactor inside the list and then outside the list
    codemodListVal :: Type l (f t) -> Type l t -> ConstVal a -> ConstVal a
    codemodListVal outer inner c@ListConst{..} = f progEnv outer
      c { lvElems = [ e { leElem = codemodConst inner leElem }
                    | e@ListElem{..} <- lvElems
                    ]
        }
    codemodListVal t _ c = f progEnv t c

    codemodMapVal
      :: Type l (f k v) -> Type l k -> Type l v -> ConstVal a -> ConstVal a
    codemodMapVal outer k v m@MapConst{..} = f progEnv outer
      m { mvElems =
           [ e { leElem = mp { mpKey = codemodConst k mpKey
                             , mpVal = codemodConst v mpVal
                             }
               }
           | e@ListElem{leElem=mp@MapPair{..}} <- mvElems
           ]
        }
    codemodMapVal t _ _ c = f progEnv t c

    codemodSchema :: SCHEMA l t s -> ConstVal a -> ConstVal a
    codemodSchema schema m@MapConst{..} =
      m { mvElems =
             [ e { leElem = mp { mpVal =
                                    codemodSchemaField schema s mpVal
                               }
                 }
             | e@ListElem
                 {leElem=
                     mp@MapPair{mpKey=UntypedConst{ucConst=StringConst s _},..}}
                 <- mvElems
             ]
        }
    codemodSchema _ c = c

    codemodSchemaField
      :: SCHEMA l t s -> Text -> UntypedConst a -> UntypedConst a
    codemodSchemaField SEmpty _ c = c
    codemodSchemaField (SField _ name ty schema) fl c
      | name == fl = codemodConst ty c
      | otherwise  = codemodSchemaField schema fl c
    codemodSchemaField (SReqField _ name ty schema) fl c
      | name == fl = codemodConst ty c
      | otherwise  = codemodSchemaField schema fl c
    codemodSchemaField (SOptField _ name ty schema) fl c
      | name == fl = codemodConst ty c
      | otherwise  = codemodSchemaField schema fl c

lookupSchema :: Name -> Env l -> Maybe (Some (Schema l))
lookupSchema = wrapLookup Typechecker.lookupSchema

lookupUnion :: Name -> Env l -> Maybe (Some (USchema l))
lookupUnion = wrapLookup Typechecker.lookupUnion

lookupEnum :: Name -> Env l -> Maybe EnumValues
lookupEnum = wrapLookup Typechecker.lookupEnum

wrapLookup
  :: (ThriftName -> Loc -> Typechecker.TC l a)
  -> Name
  -> Env l
  -> Maybe a
wrapLookup f Name{..} env =
  either (const Nothing) Just $ Typechecker.runTypechecker env $
  f sourceName noLoc
