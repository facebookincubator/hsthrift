-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Thrift.Compiler.Typechecker.Monad
  ( TC, Typechecked
  , typeError, runTypechecker, mapT, mapE, emptyT, orT, ErrCollectable(..)
  , lookupType, lookupSchema, lookupUnion, lookupEnum, lookupConst
  , lookupService, lookupEnumInt
  , TypeError(..), ErrorMsg(..), AnnotationPlacement(..)
  , orderError
  ) where

import Prelude hiding (Enum)
import Data.Int (Int32)
import Data.Some
import Data.Text (Text)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Thrift.Compiler.Parser
import Thrift.Compiler.Types

-- Typechecking Monad ----------------------------------------------------------

-- The typechecking monad is a reader transformed either monad. This allows us
-- to keep track of state and errors
type TC l = ReaderT (Env l) (Either [TypeError l])

type Typechecked (l :: *) (t :: Status -> * -> * -> *) =
  TC l (t 'Resolved l Loc)

typeError :: Loc -> ErrorMsg l -> TC l a
typeError loc msg = lift $ Left [TypeError loc msg]

runTypechecker :: Env l -> TC l a -> Either [TypeError l] a
runTypechecker = flip runReaderT

-- | map a typechecking computation over a list and collect all of the type
-- errors. This differs from mapM and mapA because those will terminate with
-- the first type error
mapT :: (a -> TC l b) -> [a] -> TC l [b]
mapT f xs = ReaderT $ \env -> mapE (runTypechecker env . f) xs

mapE :: (a -> Either [e] b) -> [a] -> Either [e] [b]
mapE f = foldr (\x xs -> (:) <$> f x `collect` xs) (Right [])

-- | This is the identity for 'orT', and morally @empty@ from Alternative
--
-- This produces an empty list of errors, so should never be used as the
-- first parater of 'orT'.
emptyT :: TC l a
emptyT = lift (Left [])

-- | Try to run the first typechecker computation, but use the second if it
-- fails.  If both fail then use both errors. See 'emptyT' for identity.
-- Note: this is associative and morally @<|>@ from Alternative
orT :: TC l a -> TC l a -> TC l a
orT t1 t2 = do
  env <- ask
  lift $ case runTypechecker env t1 of
    Left err1 -> case runTypechecker env t2 of
      Left err2 -> Left (err1 <> err2)
      x2@Right{} -> x2
    x1@Right{} -> x1

-- | Similar to <*>, but doesn't obey the monad law that ap == (<*>). This
-- allows us to collect type errors from *both* parts of the computation in the
-- event that they both fail. (<*>) will only take the first failure it finds.
class ErrCollectable m where
  collect :: m (a -> b) -> m a -> m b
infixl 4 `collect`

instance Monoid e => ErrCollectable (Either e) where
  collect (Left e1) (Left e2) = Left $ e1 <> e2
  collect (Left e)  (Right _) = Left e
  collect (Right _) (Left e)  = Left e
  collect (Right f) (Right x) = Right $ f x

instance ErrCollectable m => ErrCollectable (ReaderT a m) where
  collect (ReaderT f) (ReaderT x) = ReaderT $ \env -> f env `collect` x env

-- Lookup Functions ------------------------------------------------------------

lookupType :: ThriftName -> Loc -> TC l (Some (Type l))
lookupType = envCtxLookup typeMap UnknownType

lookupSchema :: ThriftName -> Loc -> TC l (Some (Schema l))
lookupSchema = envLookup schemaMap UnknownType

lookupUnion :: ThriftName -> Loc -> TC l (Some (USchema l))
lookupUnion = envLookup unionMap UnknownType

lookupEnum :: ThriftName -> Loc -> TC l EnumValues
lookupEnum = envLookup enumMap UnknownType

-- | Allow enum values as default values for int fields.
-- This is a type violation.  This is added to allow parsing these old files
-- when @--lenient@ mode is active (see 'Options.optsLenient').
lookupEnumInt :: ThriftName -> Loc -> TC l (Maybe Int32)
lookupEnumInt = envLookup enumInt UnknownConst

-- | Loc parameter of usage of ThriftName is for error messages.
-- Loc in result is where ThriftName was defined.
lookupConst :: ThriftName -> Loc -> TC l (Some (Type l), Name, Loc)
lookupConst = envCtxLookup constMap UnknownConst

-- | Loc parameter of usage of ThriftName is for error messages.
-- Loc in result is where ThriftName was defined.
lookupService :: ThriftName -> Loc -> TC l (Name, Set.Set Text, Loc)
lookupService = envLookup serviceMap UnknownService

envLookup
  :: (Env l -> Map.Map Text u)
  -> (ThriftName -> ErrorMsg l)
  -> ThriftName
  -> Loc
  -> TC l u
envLookup getMap mkError name loc = do
  env <- ask
  case doLookup env name of
    Nothing -> typeError loc $ mkError name
    Just u  -> pure u
  where
    doLookup env (UName n) = Map.lookup n (getMap env)
    doLookup env (QName m n) =
      Map.lookup n . getMap =<< Map.lookup m (importMap env)

envCtxLookup
  :: (Env l -> Context u)
  -> (ThriftName -> ErrorMsg l)
  -> ThriftName
  -> Loc
  -> TC l u
envCtxLookup getMap mkError name loc = do
  env <- ask
  case doLookup env name of
    Nothing -> typeError loc $ mkError name
    Just u  -> pure u
  where
    doLookup env (UName n) = Map.lookup n (cMap $ getMap env)
    doLookup env (QName m n) =
      Map.lookup n . cMap . getMap =<< Map.lookup m (importMap env)

-- Type Errors -----------------------------------------------------------------

data TypeError l
  -- Intra-Modular Type Errors
  = TypeError Loc (ErrorMsg l)
  -- Inter-Modular Errors
  | EmptyInput
  | CyclicModules [ThriftFile SpliceFile Loc]

data ErrorMsg l
  = CyclicTypes [Parsed Decl]
  | CyclicServices [Parsed Service]
  | UnknownType ThriftName
  | UnknownConst ThriftName
  | UnknownService ThriftName
  | forall t. LiteralMismatch (Type l t) (UntypedConst Loc)
  | forall u v. IdentMismatch (Type l u) (Type l v) ThriftName
  | UnknownField Text
  | MissingField Text
  | InvalidFieldId Text Int
  | InvalidField (UntypedConst Loc)
  | InvalidUnion Name Int
  | EmptyUnion Text
  | forall t. InvalidThrows (Type l t) Text
  | AnnotationMismatch (AnnotationPlacement l) (Annotation Loc)
  | DuplicateName Text
  | DuplicateEnumVal Text [Text] Int
  | forall t1 t2. TypeMismatch (Type l t1) (Type l t2)
  | forall t. NotDefinedBeforeUsed (Type l t)

data AnnotationPlacement l
  = forall t. AnnType (Type l t)
  | AnnField | AnnStruct | AnnUnion | AnnTypedef | AnnEnum | AnnPriority

orderError :: TypeError l -> TypeError l -> Ordering
-- EmptyInput always comes first
orderError EmptyInput EmptyInput = EQ
orderError EmptyInput _ = LT
orderError _ EmptyInput = GT
-- CyclicModules comes next
orderError CyclicModules{} CyclicModules{} = EQ
orderError CyclicModules{} _ = LT
orderError _ CyclicModules{} = GT
-- Type Errors are ordered by Loc
orderError (TypeError l1 _) (TypeError l2 _) = compare l1 l2
