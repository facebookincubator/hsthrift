-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Thrift.Compiler.Types
  ( Status(..), IfResolved, Parsed
  , Program(..), Header(..), IncludeType(..), Decl(..), SpliceFile
  , Loc(..), noLoc, nlc, Comment(..), Located(..)
  , Separator(..), getSepLoc
  , Annotations(..), Annotation(..), AnnValue(..), getAnns, annLoc
  , StructuredAnnotation(..), StructuredAnnotationElems(..)
  , ThriftPriority(..)
  , Env(..), emptyEnv, Context(..), emptyContext
  , TypeMap, SchemaMap, UnionMap, EnumMap, ConstMap, ServiceMap, ImportMap
  , EnumValues, EnumInt
  , Typedef(..), TypedefTag(..), TypedefLoc(..)
  , Const(..), ConstLoc(..)
  , UntypedConst(..), ConstVal(..), ListElem(..), MapPair(..), QuoteType(..)
  , StructPair (..)
  , TypedConst(..)
  , List(..), Set(..), HashSet(..), Map(..), HashMap(..), EnumVal(..), New(..)
  , StructVal(..), ExceptionVal(..), UnionVal(..), MembershipProof(..)
  , Struct(..), StructType(..), StructLoc(..)
  , Field(..), FieldLoc(..), FieldType(..), FieldTag(..), FieldId
  , Requiredness(..), Laziness(..)
  , Union(..), UnionAlt(..), PossiblyEmpty(..), EmptyName
  , Enum(..), EnumValue(..), EnumValLoc(..)
  , Service(..), Super(..), Function(..), FunLoc(..), ThrowsLoc(..)
  , Type, AnnotatedType(..), TType(..), SomeAnnTy(..)
  , TypeLoc(..), GetArity, getTypeLoc
  , SCHEMA(..), Schema, USchema
  , SpecialType
  , Name(..), ThriftName, Name_(..), localName, mapName, mkName
  ) where

import Prelude hiding (Enum)
import Data.ByteString (ByteString)
import Data.Int
import Data.Proxy
import Data.Some
import Data.Text (Text)
import GHC.TypeLits
import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax (Module)

import Thrift.Compiler.Options

data Status = Resolved | Unresolved

type Parsed t = t 'Unresolved () Loc

type family IfResolved (s :: Status) (t :: *) :: * where
  IfResolved 'Unresolved t = ()
  IfResolved 'Resolved   t = t

data ThriftPriority
  = HighImportant
  | High
  | Important
  | NormalPriority
  | BestEffort
  | NPriorities
  deriving (Show, Eq)

-- Thrift Program --------------------------------------------------------------

data Program (l :: * {- Language -}) a = Program
  { progName      :: Text           -- ^ Thrift name
  , progHSName    :: Text           -- ^ Haskell module prefix (with namespace)
  , progPath      :: FilePath
  , progOutPath   :: FilePath
  , progInstances :: SpliceFile
  , progIncludes  :: [Program l a]
  , progHeaders   :: [Header a]
  , progDecls     :: [Decl 'Resolved l a]
  , progComments  :: [Comment a]
  , progEnv       :: Env l
  }

data Header a
  = HInclude
    { incPath       :: FilePath
    , incType       :: IncludeType
    , incKeywordLoc :: Located a
    , incPathLoc    :: Located a
    , incQuoteType  :: QuoteType
    }
  | HNamespace
    { nmLang       :: Text
    , nmName       :: Text
    , nmKeywordLoc :: Located a
    , nmLangLoc    :: Located a
    , nmNameLoc    :: Located a
    , nmQuoteType  :: Maybe QuoteType
    }

data IncludeType = Include | HsInclude | CppInclude

data Decl s l a
  = D_Struct (Struct s l a)
  | D_Union (Union s l a)
  | D_Enum (Enum s l a)
  | D_Typedef (Typedef s l a)
  | D_Const (Const s l a)
  | D_Service (Service s l a)

type SpliceFile = Maybe (Module SrcSpanInfo)

data Annotations a = Annotations
  { annList :: [Annotation a]
  , annOpenParen  :: Located a
  , annCloseParen :: Located a
  }

data Annotation a
  = SimpleAnn
    { saTag :: Text
    , saLoc :: Located a
    , saSep :: Separator a
    }
  | ValueAnn
    { vaTag :: Text
    , vaVal :: AnnValue
    , vaTagLoc :: Located a
    , vaEqual  :: Located a
    , vaValLoc :: Located a
    , vaSep    :: Separator a
    }

data AnnValue = IntAnn Int Text | TextAnn Text QuoteType

getAnns :: Maybe (Annotations a) -> [Annotation a]
getAnns = maybe [] annList

annLoc :: Annotation a -> a
annLoc SimpleAnn{..} = lLocation saLoc
annLoc ValueAnn{..} = lLocation vaTagLoc

data StructuredAnnotation (s :: Status) (l :: *) a = forall t. StructuredAnn
  { saAt :: Located a
  , saType :: Text
  , saTypeLoc :: TypeLoc 0 a
  , saMaybeElems :: Maybe (StructuredAnnotationElems a)
  , saResolvedType :: IfResolved s (Type l t)
  , saResolvedVal :: IfResolved s (Some (StructVal l))
  }

data StructuredAnnotationElems a = StructuredAnnElems
  { saOpenBrace  :: Located a
  , saElems      :: [ListElem StructPair a]
  , saCloseBrace :: Located a
  }

data Loc = Loc
  { locFile :: FilePath
  , locStartLine :: Int
  , locStartCol  :: Int
  , locEndLine   :: Int
  , locEndCol    :: Int
  } deriving (Show, Eq)

instance Ord Loc where
  l1 <= l2
    | locFile l1 == locFile l2 =
        if locStartLine l1 == locStartLine l2
        then locStartCol l1 <= locStartCol l2
        else locStartLine l1 <= locStartLine l2
    | otherwise = locFile l1 <= locFile l2

noLoc :: Loc
noLoc = Loc "" 0 0 0 0

nlc :: Located Loc
nlc = Located [] noLoc

data Comment a = Comment a Text
  deriving (Show, Eq)

data Located a = Located
  { lComments :: [Comment a]
  , lLocation :: a
  } deriving (Show, Eq)

data Separator a
  = Semicolon (Located a)
  | Comma (Located a)
  | NoSep

getSepLoc :: Separator a -> Maybe (Located a)
getSepLoc (Semicolon loc) = Just loc
getSepLoc (Comma loc) = Just loc
getSepLoc NoSep = Nothing

-- Environment -----------------------------------------------------------------

-- | The type checking environment keeps track of
data Env (l :: * {- Language -}) = Env
  { typeMap    :: TypeMap l   -- ^ Local Named Types
  , schemaMap  :: SchemaMap l -- ^ Local Schemas
  , unionMap   :: UnionMap l  -- ^ Local Union Schemas
  , enumMap    :: EnumMap     -- ^ Local Enums
  , enumInt    :: EnumInt
    -- ^ Local Enums as Int for default values, weird mode
  , constMap   :: ConstMap l  -- ^ Local Constants
  , serviceMap :: ServiceMap  -- ^ Local Services
  , importMap  :: ImportMap l -- ^ all the modules that are in scope
  , options    :: Options l   -- ^ Program options
  , envName    :: Text        -- ^ The 'thriftName' of this 'Env'
  }

-- | Help build a specialized 'Env' for 'runTypechecker' while
-- 'typecheckModule' is constructing the final 'Env' and 'Program'
emptyEnv :: (Text, Options l) -> Env l
emptyEnv (thriftName, opts) = Env
  { typeMap   = emptyContext
  , schemaMap = Map.empty
  , unionMap  = Map.empty
  , enumMap   = Map.empty
  , enumInt   = Map.empty
  , constMap  = emptyContext
  , serviceMap = Map.empty
  , importMap = Map.empty
  , options   = opts
  , envName   = thriftName
  }

data Context a = Context
  { cMap   :: Map.Map Text a -- ^ Map to lookup value of a symbol
  , cScope :: Set.Set Text   -- ^ Additional set of symbols that are in scope
                             -- ie hsNames for types and constants
  }

emptyContext :: Context a
emptyContext = Context
  { cMap   = Map.empty
  , cScope = Set.empty
  }

type TypeMap l   = Context (Some (Type l))
type SchemaMap l = Map.Map Text (Some (Schema l))
type UnionMap l  = Map.Map Text (Some (USchema l))
type EnumMap     = Map.Map Text EnumValues
-- For Constants, we store the renamed version in addition to the type
type ConstMap l  = Context (Some (Type l), Name, Loc)
-- For Service, we store the Set Text of renamed functions for scoping
type ServiceMap  = Map.Map Text (Name, Set.Set Text, Loc)
type ImportMap l = Map.Map Text (Env l)

-- We need to be able to look up the constants keyed by their numeric value
-- or themselves
type EnumValues = (Map.Map Int32 (Name, Loc), Map.Map Text (Name, Loc))

-- | There are weird thrift file using enum constants as I32 and I64 defaults,
-- (as unqualified names).  If there is a name collision with different value
-- this holds Nothing to detect the ambiguity. Active when weird mode is on.
type EnumInt = Map.Map Text (Maybe Int32)

-- Thrift Typedef --------------------------------------------------------------

data Typedef (s :: Status) (l :: * {- Language -}) a = forall v t. Typedef
  { tdName         :: Text
  , tdTag          :: TypedefTag s
  , tdResolvedName :: IfResolved s Text
  , tdType         :: AnnotatedType a v
  , tdResolvedType :: IfResolved s (Type l t)
  , tdLoc          :: TypedefLoc a
  , tdAnns         :: Maybe (Annotations a)
  , tdSAnns        :: [StructuredAnnotation s l a]
  }

data TypedefTag (s :: Status) where
  IsTypedef :: TypedefTag s
  IsNewtype :: TypedefTag 'Resolved

data TypedefLoc a = TypedefLoc
  { tdlKeyword :: Located a
  , tdlName    :: Located a
  }

-- Thrift Constant--------------------------------------------------------------

data Const (s :: Status) (l :: * {- Language -}) a = forall v t. Const
  { constName         :: Text
  , constResolvedName :: IfResolved s Text
  , constType         :: AnnotatedType a v
  , constVal          :: UntypedConst a
  , constResolvedType :: IfResolved s (Type l t)
  , constResolvedVal  :: IfResolved s (TypedConst l t)
  , constLoc          :: ConstLoc a
  , constSAnns        :: [StructuredAnnotation s l a]
  }

data ConstLoc a = ConstLoc
  { clKeyword   :: Located a
  , clName      :: Located a
  , clEqual     :: Located a
  , clSeparator :: Separator a
  }

data UntypedConst a = UntypedConst
  { ucLoc   :: Located a
  , ucConst :: ConstVal a
  }

data ConstVal a
  = IntConst Int Text
  -- ^ Includes are numeric types, enums, and booleans
  | DoubleConst Double Text
  -- ^ Doubles and Floats
  | StringConst Text QuoteType
  | IdConst Text
  | ListConst
    { lvElems      :: [ListElem UntypedConst a]
    , lvCloseBrace :: Located a
    }
  -- ^ Lists and Sets
  | MapConst
    { mvElems      :: [ListElem MapPair a]
    , mvCloseBrace :: Located a
    }
  -- ^ Maps and Structs
  | StructConst
    { svType       :: Text
    , svOpenBrace  :: Located a
    , svElems      :: [ListElem StructPair a]
    , svCloseBrace :: Located a
    }
  | BoolConst Bool

data ListElem t a = ListElem
  { leElem :: t a
  , leSeparator :: Separator a
  }

data MapPair a = MapPair
  { mpKey   :: UntypedConst a
  , mpColon :: Located a
  , mpVal   :: UntypedConst a
  }

data StructPair a = StructPair
  { spKey :: Text
  , spKeyLoc :: Located a
  , spEquals:: Located a
  , spVal:: UntypedConst a
  }

data QuoteType = SingleQuote | DoubleQuote
  deriving (Show, Eq)

data TypedConst l t
  = Literal t
  | Identifier Name (Type l t) Loc -- ^ Loc for definition of Name
  | WeirdEnumToInt (Type l t) Name (Type l EnumVal) Loc

newtype List l t      = List [TypedConst l t]
newtype Set l t       = Set [TypedConst l t]
newtype HashSet l t   = HashSet [TypedConst l t]
newtype Map l k v     = Map [(TypedConst l k, TypedConst l v)]
newtype HashMap l k v = HashMap [(TypedConst l k, TypedConst l v)]

-- | Include Loc of definition (from EnumValLoc)
data EnumVal = EnumVal Name Loc

newtype New t = New t

-- The fields of a struct are encoded in the type parameter of the StructVal
-- s has kind [(Symbol, *)]. The Symbol is a type-level string, which is the
-- name of the field and the * is the actual type of the field. A list of these
-- pairs makes a StructVal
data StructVal (l :: * {- Language -}) (s :: [(Symbol, *)]) where
  Empty :: StructVal l '[]

  ConsVal
    :: forall (name :: Symbol) l t s. KnownSymbol name
    => Proxy name
    -> Type l t
    -> TypedConst l t
    -> StructVal l s
    -> StructVal l ('(name, t) ': s)
  ConsDefault
    :: forall (name :: Symbol) l t s. KnownSymbol name
    => Proxy name
    -> Type l t
    -> StructVal l s
    -> StructVal l ('(name, t) ': s)
  ConsJust
    :: forall (name :: Symbol) l t s. KnownSymbol name
    => Proxy name
    -> Type l t
    -> TypedConst l t
    -> StructVal l s
    -> StructVal l ('(name, Maybe t) ': s)
  ConsNothing
    :: forall (name :: Symbol) l t s. KnownSymbol name
    => Proxy name
    -> StructVal l s
    -> StructVal l ('(name, Maybe t) ': s)

newtype ExceptionVal l s = EV (StructVal l s)

-- Unions have exactly one value out of a list of possible values. However, they
-- also need to include a MembershipProof which proves that the present field is
-- a member of the union's schema
data UnionVal (l :: * {- Language -}) (s :: [(Symbol, *)]) where
  UnionVal
    :: forall (name :: Symbol) l s t. KnownSymbol name
    => Proxy name
    -> Type l t
    -> TypedConst l t
    -> MembershipProof '(name, t) s
    -> UnionVal l s

-- Proof that a field is a memeber of a schema
data MembershipProof x xs where
  -- The field is the first field in the type list
  PHere  :: MembershipProof x (x ': xs)
  -- The field is buried somewhere deeper in the type list
  PThere :: MembershipProof x xs -> MembershipProof x (y ': xs)

-- Thrift Structs and Exceptions -----------------------------------------------

data Struct s l a = Struct
  { structName         :: Text
  , structResolvedName :: IfResolved s Text
  , structType         :: StructType
  , structMembers      :: [Field 'StructField s l a]
  , structLoc          :: StructLoc a
  , structAnns         :: Maybe (Annotations a)
  , structSAnns        :: [StructuredAnnotation s l a]
  }

data StructType = StructTy | ExceptionTy

data StructLoc a = StructLoc
  { slKeyword    :: Located a
  , slName       :: Located a
  , slOpenBrace  :: Located a
  , slCloseBrace :: Located a
  }

-- Thrift Fields ---------------------------------------------------------------

data FieldType = StructField | Argument | Throws

data FieldTag u s l t where
  STRUCT_FIELD :: FieldTag 'StructField s l t
  ARGUMENT     :: FieldTag 'Argument s l t
  THROWS_UNRESOLVED :: FieldTag 'Throws 'Unresolved l t
  THROWS_RESOLVED   :: FieldTag 'Throws 'Resolved l (Some (ExceptionVal l))

data Field u s l a = forall v t. Field
  { fieldId           :: FieldId
  , fieldName         :: Text
  , fieldResolvedName :: IfResolved s Text
  , fieldType         :: AnnotatedType a v
  , fieldResolvedType :: IfResolved s (Type l t)
  , fieldVal          :: Maybe (UntypedConst a)
  , fieldResolvedVal  :: IfResolved s (Maybe (TypedConst l t))
  , fieldRequiredness :: Requiredness u a
  , fieldLaziness     :: Laziness
  , fieldTag          :: FieldTag u s l t
  , fieldLoc          :: FieldLoc a
  , fieldAnns         :: Maybe (Annotations a)
  , fieldSAnns        :: [StructuredAnnotation s l a]
  }

type FieldId = Int32

data Requiredness u a where
  Default :: Requiredness u a

  -- Optional and Required are only for Struct Fields (not arguments)
  Optional :: Located a -> Requiredness 'StructField a
  Required :: Located a -> Requiredness 'StructField a

data Laziness = Lazy | Strict

data FieldLoc a = FieldLoc
  { flId        :: Located a
  , flIdRep     :: Text
  , flColon     :: Located a
  , flName      :: Located a
  , flEqual     :: Maybe (Located a)
  , flSeparator :: Separator a
  }

-- Thrift Unions ---------------------------------------------------------------

data Union s l a = forall u. Union
  { unionName         :: Text
  , unionResolvedName :: IfResolved s Text
  , unionAlts         :: [UnionAlt s l a]
  , unionEmptyName    :: EmptyName s u
  , unionHasEmpty     :: PossiblyEmpty u
  , unionLoc          :: StructLoc a
  , unionAnns         :: Maybe (Annotations a)
  , unionSAnns        :: [StructuredAnnotation s l a]
  }

data UnionAlt (s :: Status) (l :: * {- Language -}) a = forall v t. UnionAlt
  { altId           :: FieldId
  , altName         :: Text
  , altResolvedName :: IfResolved s Text
  , altType         :: AnnotatedType a v
  , altResolvedType :: IfResolved s (Type l t)
  , altLoc          :: FieldLoc a
  , altAnns         :: Maybe (Annotations a)
  , altSAnns        :: [StructuredAnnotation s l a]
  }

data PossiblyEmpty (u :: Bool) where
  HasEmpty :: PossiblyEmpty 'True
  NonEmpty :: PossiblyEmpty 'False

type family EmptyName s u where
  EmptyName 'Unresolved u = ()
  EmptyName 'Resolved 'True  = Text
  EmptyName 'Resolved 'False = ()

-- Thrift Enums ----------------------------------------------------------------

data Enum (s :: Status) (l :: * {- Language -}) a = Enum
  { enumName         :: Text
  , enumResolvedName :: IfResolved s Text
  , enumIsPseudo     :: IfResolved s Bool
  , enumConstants    :: [EnumValue s l a]
  , enumLoc          :: StructLoc a
  , enumAnns         :: Maybe (Annotations a)
  , enumSAnns        :: [StructuredAnnotation s l a]
  , enumNoUnknown    :: IfResolved s Bool
  }

data EnumValue (s :: Status) (l :: * {- Language -}) a = EnumValue
  { evName         :: Text
  , evResolvedName :: IfResolved s Text
  , evValue        :: Int32
  , evLoc          :: EnumValLoc a
  , evAnns         :: Maybe (Annotations a)
  , evSAnns        :: [StructuredAnnotation s l a]
  }

data EnumValLoc a = EnumValLoc
  { evlName      :: Located a
  , evlEqual     :: Located a
  , evlValue     :: Located a
  , evlRep       :: Text
  , evlSeparator :: Separator a
  }

-- Thrift Service --------------------------------------------------------------

data Service (s :: Status) (l :: * {- Language -}) a = Service
  { serviceName         :: Text
  , serviceResolvedName :: IfResolved s Text
  , serviceSuper        :: Maybe (Super s a)
  , serviceFunctions    :: [Function s l a]
  , serviceLoc          :: StructLoc a
  , serviceAnns         :: Maybe (Annotations a)
  , serviceSAnns        :: [StructuredAnnotation s l a]
  }

data Super s a = Super
  { supName         :: Text
  , supResolvedName :: IfResolved s (Name, Loc)
  , supExtends      :: Located a -- ^ location of "extends" keyword
  , supLoc          :: Located a -- ^ location of "supName" after "extends"
  }

type family SuperOf (s :: Status) :: * where
  SuperOf 'Resolved   = Name
  SuperOf 'Unresolved = Text

data Function (s :: Status) (l :: * {- Language -}) a = Function
  { funName         :: Text
  , funResolvedName :: IfResolved s Text
  , funType         :: Either (Located a) (Some (AnnotatedType a))
  , funResolvedType :: IfResolved s (Maybe (Some (Type l)))
  , funArgs         :: [Field 'Argument s l a]
  , funExceptions   :: [Field 'Throws s l a]
  , funIsOneWay     :: Bool
  , funPriority     :: ThriftPriority
  , funLoc          :: FunLoc a
  , funAnns         :: Maybe (Annotations a)
  , funSAnns        :: [StructuredAnnotation s l a]
  }

data FunLoc a = FunLoc
  { fnlOneway     :: Maybe (Located a)
  , fnlName       :: Located a
  , fnlOpenParen  :: Located a
  , fnlCloseParen :: Located a
  , fnlThrows     :: Maybe (ThrowsLoc a)
  , fnlSeparator  :: Separator a
  }

data ThrowsLoc a = ThrowsLoc
  { tlThrows     :: Located a
  , tlOpenParen  :: Located a
  , tlCloseParen :: Located a
  }

-- Thrift Value Types ----------------------------------------------------------

-- When a thrift file is parsed, all of the types are marked as 'Unresolved
-- meaning that we don't know have any information about the named types.
-- During type checking, these types become 'Resolved, a process by which all
-- of the named types become either structs, enums, or typedefs
type Type l = TType 'Resolved l Loc

-- Annotations are resolved during typechecking, so we don't need them once
-- types are resolved
type family TypeOf (s :: Status) (l :: * {- Language -}) a :: * -> * where
  TypeOf 'Resolved l a   = Type l
  TypeOf 'Unresolved l a = AnnotatedType a

data AnnotatedType a t = AnnotatedType
  { atType        :: TType 'Unresolved () a t
  , atAnnotations :: Maybe (Annotations a)
  , atLoc         :: TypeLoc (GetArity t) a
  }

type family GetArity t :: Nat where
  GetArity (List l a) = 1
  GetArity (Set l a) = 1
  GetArity (HashSet l a) = 1
  GetArity (Map l a b) = 2
  GetArity (HashMap l a b) = 2
  GetArity a = 0

data TypeLoc (n :: Nat) a where
  Arity0Loc :: { a0Ty :: Located a } -> TypeLoc 0 a
  Arity1Loc
    :: { a1Ty         :: Located a
       , a1OpenBrace  :: Located a
       , a1CloseBrace :: Located a
       }
    -> TypeLoc 1 a
  Arity2Loc
    :: { a2Ty         :: Located a
       , a2OpenBrace  :: Located a
       , a2Comma      :: Located a
       , a2CloseBrace :: Located a
       }
    -> TypeLoc 2 a

getTypeLoc :: TypeLoc n a -> a
getTypeLoc Arity0Loc{..} = lLocation a0Ty
getTypeLoc Arity1Loc{..} = lLocation a1Ty
getTypeLoc Arity2Loc{..} = lLocation a2Ty

data SomeAnnTy s l =
  forall t. ThisAnnTy (TType s l Loc t) (TypeLoc (GetArity t) Loc)

data TType (s :: Status) (l :: * {- Language -}) a (t :: *) where
  -- Integral TTypes
  I8  :: TType s l a Int8
  I16 :: TType s l a Int16
  I32 :: TType s l a Int32
  I64 :: TType s l a Int64

  -- Floating Point TTypes
  TFloat  :: TType s l a Float
  TDouble :: TType s l a Double

  -- Other Base TTypes
  TBool   :: TType s l a Bool
  TText   :: TType s l a Text
  TBytes  :: TType s l a ByteString

  -- Collections
  TSet     :: TypeOf s l a t -> TType s l a (Set l t)
  THashSet :: TypeOf s l a t -> TType s l a (HashSet l t)
  TList    :: TypeOf s l a t -> TType s l a (List l t)
  TMap     :: TypeOf s l a k -> TypeOf s l a v -> TType s l a (Map l k v)
  THashMap :: TypeOf s l a k -> TypeOf s l a v -> TType s l a (HashMap l k v)

  -- TTypedefs
  TTypedef :: Name -> Type l t -> Loc -> TType 'Resolved l a t
  TNewtype :: Name -> Type l t -> Loc -> TType 'Resolved l a (New t)

  -- Structs
  TStruct    :: Name -> Loc -> TType 'Resolved l a (Some (StructVal l))
  TException :: Name -> Loc -> TType 'Resolved l a (Some (ExceptionVal l))
  TUnion     :: Name -> Loc -> TType 'Resolved l a (Some (UnionVal l))

  -- Enums
  TEnum   :: Name -> Loc -> Bool -> TType 'Resolved l a EnumVal

  -- Unresolved named types
  TNamed :: Text -> TType 'Unresolved l a ()

  -- Special types that we get by resolving annotations
  TSpecial :: SpecialType l t -> TType 'Resolved l a t

data family SpecialType (l :: * {- Language -}) (t :: *)

data SchemaType = StructSchema | UnionSchema

type Schema l = SCHEMA l 'StructSchema
type USchema l = SCHEMA l 'UnionSchema

-- Schema for a struct, defines exactly what must be present
data SCHEMA (l :: * {- Language -}) (t :: SchemaType) (s :: [(Symbol, *)]) where
  -- | Empty struct has type empty list
  SEmpty :: SCHEMA l u '[]

  SField
    :: forall (name :: Symbol) l t s. KnownSymbol name
    => Proxy name
    -> Text
    -> Type l t
    -> SCHEMA l 'StructSchema s
    -> SCHEMA l 'StructSchema ('(name, t) ': s)

  -- | For required fields, the value must be present
  SReqField
    :: forall (name :: Symbol) l u t s. KnownSymbol name
    => Proxy name
    -> Text
    -> Type l t
    -> SCHEMA l u s
    -> SCHEMA l u ('(name, t) ': s)

  -- | For optional fields, the value is a Maybe
  -- Only structs can have optional fields
  SOptField
     :: forall (name :: Symbol) l t s. KnownSymbol name
     => Proxy name
     -> Text
     -> Type l t
     -> SCHEMA l 'StructSchema s
     -> SCHEMA l 'StructSchema ('(name, Maybe t) ': s)

-- | Renamed for target langauge
data Name = Name
  { sourceName   :: ThriftName
  , resolvedName :: Name_ 'Resolved
  } deriving (Eq)

-- | Name in the original thrift source file
type ThriftName = Name_ 'Unresolved

data Name_ (s :: Status)
  = UName Text
  | QName Text Text
  deriving (Eq)

localName :: Name_ s -> Text
localName (UName n) = n
localName (QName _ n) = n

mapName :: (Text -> Text) -> Name_ s -> Name_ s
mapName f (UName n) = UName $ f n
mapName f (QName m n) = QName m $ f n

mkName :: Text -> Text -> Name
mkName tname rname = Name
  { sourceName = UName tname
  , resolvedName = UName rname
  }
