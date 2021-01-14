-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ViewPatterns #-}

module Mangle
  ( mangle
  ) where

import Control.Arrow
import Control.Applicative hiding (Const)
import Control.Monad
import Data.Functor.Identity
import Data.Function
import Data.List
import Data.Maybe
import Data.Set (Set)
import Foreign.C
import Numeric
import Text.Parsec hiding ((<|>), many)

import qualified Data.Set as Set

-- | Mangles a C++ signature into a string.
mangle :: String -> Either ParseError String
mangle = fmap show . runParser sig (Uniq 0) ""

foreign export ccall itaniumMangle :: CString -> CSize -> IO CString
itaniumMangle :: CString -> CSize -> IO CString
itaniumMangle csymbol clen = do
  symbol <- peekCStringLen (csymbol, fromIntegral clen)
  either (\e -> fail $ "itaniumMangle failed: " ++ symbol ++ "\n" ++ show e)
         newCString
         (mangle symbol)

--------------------------------------------------------------------------------
-- Signatures
--
-- A signature consists of a name, a parameter list, and a set of
-- cv-qualifiers. The return type is parsed, but is not included in the mangled
-- name, so we don't store it.

data Sig = Sig Name [Type] (Set CV)

instance Show Sig where
  show (normalizeSig -> s@(Sig sigName params cv)) = concat
    $ "_Z"  -- All mangled symbols start this way.
    : showCvName cv sigName
    : if null params
      then [showType s (Builtin Void)]
      else map (showType s) params
    where
    -- cv-qualifiers aren't allowed on non-nested names in C++.
    showCvName _ (Unqual n _) = lengthPrefix n
    showCvName cvs (Qual names name _)
      = lengthEncode (showCvs cvs) "" (names ++ [name])

sig :: Parser Sig
sig = do
  spaces
  _ <- type_  -- Return type, ignored.
  id_ <- nestedId
  params_ <- list type_
  cvs <- opt cvQuals
  return $ Sig id_ params_ cvs

normalizeSig :: Sig -> Sig
normalizeSig (Sig name params cv) =
  Sig name (map normalizeParameterType params) cv

--------------------------------------------------------------------------------
-- Types

data Type
  = Builtin Builtin
  | Named Name (Maybe [Type]) Uniq
  | Ptr Type Uniq
  | Ref Type Uniq
  | CV (Set CV) Type Uniq
  deriving (Show)

instance Eq Type where
  Builtin a == Builtin b = a == b
  Named a b _ == Named c d _ = (a, b) == (c, d)
  Ptr a _ == Ptr b _ = a == b
  Ref a _ == Ref b _ = a == b
  CV a b _ == CV c d _ = (a, b) == (c, d)
  _ == _ = False

showType :: Sig -> Type -> String
showType s t = case t of
  Builtin b -> show b
  Named name args _ -> (showName name args' `fromMaybe` findName name args)
    where args' = maybe "" (("I" ++) . (++ "E") . concatMap recur) args
  Ptr t' u -> fromMaybe ("P" ++ recur t') (findType t u)
  Ref t' u -> fromMaybe ("R" ++ recur t') (findType t u)
  CV cvs t' u -> fromMaybe (showCvs cvs ++ recur t') (findType t u)
  where
  recur = showType s

  findType t' u = search byType id
    where
    byType match (TypeSub t'')
      = t' == t'' && maybe False (match u) (typeUniq t'')
    byType _ _ = False

  findName (Qual names name u) args = byName <|> byQual
    where
    byName = search byName' id
    byQual = search byQual'
      $ \i -> concat ["N", i, lengthPrefix name, "E"]
    byName' match (NameSub names' args' u')
      = and [names ++ [name] == names', match u u', args == args']
    byName' _ _ = False
    byQual' match (QualSub names' u')
      = names == names' && match u u'
    byQual' _ _ = False

  findName (Unqual name u) args = search byName id
    where
    byName match (NameSub names' args' u')
      = and [[name] == names', match u u', args == args']
    byName _ _ = False

  search by f = do
    -- Find a component that is equal but elsewhere.
    i <- subIndex (by (/=)) s
    -- Ensure it occurs before this one.
    guard $ i `before` subIndex (by (==)) s
    return $ f (show i)

  _ `before` Nothing = True
  i `before` Just i' = i < i'

type_ :: Parser Type
type_ = do
  -- cv-qualifiers may occur before or after base types.
  cv1 <- opt cvQuals
  id_ <- typeId
  mods <- many $ ref <|> ((\q u t -> mkCv q t u) <$> cvQuals <*> genUniq)
  u <- genUniq
  return $ foldr ($) (mkCv cv1 id_ u) (reverse mods)

typeId :: Parser Type
typeId = do
  qual <- typeQual
  parts <- if null qual
    then idParts
    else do
      ps <- optionMaybe ((:[]) <$> qualifiable)
      return $ flip fromMaybe ps $ case qual of
        -- long is a qualifier as well as a end value itself
        ["long"] -> []
        _ -> ["int"]
  args <- optionMaybe templateArgs
  case maybeBuiltin (sortBy (flip compare) $ qual ++ parts) of
    Just builtin -> return $ Builtin builtin
    -- TODO: Perhaps verify that 'qual' is empty if not used.
    Nothing -> Named <$> (case parts of
      [] -> error "empty identifier"
      [part] -> Unqual part <$> genUniq
      _ -> Qual (init parts) (last parts) <$> genUniq) <*> pure args <*> genUniq

typeQual :: Parser [String]
typeQual = fmap (sortBy $ flip compare)
  . many . choice $ map word ["long", "unsigned", "signed"]

templateArgs :: Parser [Type]
templateArgs = between (word "<") (word ">") (commas type_)

qualifiable :: Parser String
qualifiable = choice $ map word
  ["char", "int", "__int64", "__int128"]

ref :: Parser (Type -> Type)
ref = flip <$> (Ptr <$ word "*" <|> Ref <$ word "&") <*> genUniq

-- From the itanium ABI spec:
-- "Note that top-level cv-qualifiers specified on a parameter type do
-- not affect the function type directly (i.e., int(*)(T) and int(*)(T
-- const) are the same type)"
normalizeParameterType :: Type -> Type
normalizeParameterType t = case normalizeType t of
  CV _ t' _ -> t'
  other -> other

normalizeType :: Type -> Type
normalizeType t = case t of
  Builtin{} -> t
  Named name args u -> Named name (map normalizeType <$> args) u
  Ptr t' u -> Ptr (normalizeType t') u
  Ref t' u -> Ref (normalizeType t') u
  CV cvs t' _ | Set.null cvs -> normalizeType t'
  CV cvs (CV cvs' t' _) u -> normalizeType $ CV (cvs <> cvs') t' u
  CV cvs t' u -> CV cvs (normalizeType t') u

--------------------------------------------------------------------------------
-- Names

data Name = Qual [String] String Uniq | Unqual String Uniq
  deriving (Show)

instance Eq Name where
  Qual a b _ == Qual c d _ = (a, b) == (c, d)
  Unqual a _ == Unqual b _ = a == b
  _ == _ = False

idParts :: Parser [String]
idParts = rawId `sepBy1` word "::"

showName :: Name -> String -> String
showName (Unqual name _) args = lengthEncode "" args [name]
showName (Qual names name _) args = lengthEncode "" args (names ++ [name])

lengthEncode :: String -> String -> [String] -> String
lengthEncode cvs args = \case
  ["std", "allocator"] -> "Sa"
  ["std", "basic_string"] -> "Sb" ++ args
  ["std", "string"] -> "Ss"
  ["std", "istream"] -> "Si"
  ["std", "ostream"] -> "So"
  ["std", "iostream"] -> "Sd"
  ["std", "size_t"] -> "m"
  ["std", name] -> "St" ++ lengthPrefix name ++ args
  "std":names -> concat ["NSt", concatMap lengthPrefix names, args, "E"]
  [name] -> lengthPrefix name ++ args
  names -> concat ["N", cvs, concatMap lengthPrefix names, args, "E"]

lengthPrefix :: String -> String
lengthPrefix = uncurry (++) . (show . length &&& id)

nestedId :: Parser Name
nestedId = do
  parts <- idParts
  (case parts of
    [part] -> Unqual part
    _ -> Qual (init parts) (last parts)) <$> genUniq

rawId :: Parser String
rawId = notFollowedBy cvQual *> lexeme
  (liftA2 (:) nondigit (many $ nondigit <|> digit) <?> "identifier")

nondigit :: Parser Char
nondigit = letter <|> char '_'

--------------------------------------------------------------------------------
-- Substitutions
--
-- Symbols are compressed by allowing signature components to refer to prior
-- components in the signature.

data Sub
  = QualSub [String] Uniq
  | NameSub [String] (Maybe [Type]) Uniq
  | TypeSub Type
  deriving (Eq, Show)

subIndex :: (Sub -> Bool) -> Sig -> Maybe Index
subIndex f = fmap Index . findIndex f . nubBy ((==) `on` ignoreUniq) . sigSubs
  where
  ignoreUniq (QualSub names _) = QualSub names $ Uniq 0
  ignoreUniq (NameSub names name _) = NameSub names name $ Uniq 0
  ignoreUniq t = t
  -- 'nub' because substitutions are not repeated.

-- Note that the whole nested name from a signature is not considered for
-- substitution, only its prefix.
sigSubs :: Sig -> [Sub]
sigSubs (Sig Unqual{} types _)
  = concatMap typeSubs types
sigSubs (Sig (Qual names _ u) types _)
  =  [QualSub nss u | nss <- tail $ inits names]
  ++ concatMap typeSubs types

typeSubs :: Type -> [Sub]
typeSubs Builtin{} = []
typeSubs (Named (Unqual name u) args _)
  = NameSub [name] Nothing u
  : [NameSub [name] args u | isJust args]
typeSubs (Named (Qual names name u) args _)
  =  [QualSub nss u | nss <- tail $ inits names]
  ++ [NameSub names' Nothing u]
  ++ [NameSub names' args u | isJust args]
  where
  names' = names ++ [name]
typeSubs t@(Ptr t' _) = typeSubs t' ++ [TypeSub t]
typeSubs t@(Ref t' _) = typeSubs t' ++ [TypeSub t]
typeSubs t@(CV _ t' _) = typeSubs t' ++ [TypeSub t]

--------------------------------------------------------------------------------
-- Substitution indices
--
-- Backreferences to substitutions are mangled in base 36.

newtype Index = Index Int
  deriving (Bounded, Eq, Ord)

instance Show Index where
  show (Index 0) = "S_"
  show (Index n) = "S" ++ showIntAtBase 36 c (n - 1) "_"
    where
    c x = toEnum $ if x >= 0 && x <= 9
      then fromEnum '0' + x
      else fromEnum 'A' + (x - 10)

--------------------------------------------------------------------------------
-- cv-qualifiers
--
-- cv-qualifiers are ordered and deduplicated, so we store them in sets.

data CV = Volatile | Restrict | Const
  deriving (Eq, Ord)

instance Show CV where
  show = \case
    Const -> "K"
    Restrict -> "r"
    Volatile -> "V"

constQual :: Parser CV
constQual = Const <$ word "const"

cvQuals :: Parser (Set CV)
cvQuals = Set.fromList <$> many1 cvQual

cvQual :: Parser CV
cvQual = choice [constQual, volatileQual, restrictQual]

mkCv :: Set CV -> Type -> Uniq -> Type
mkCv cvs (CV cvs' t _) u = CV (cvs <> cvs') t u
mkCv cvs t u = CV cvs t u

-- Basically unnecessary.
restrictQual :: Parser CV
restrictQual = Restrict <$ word "restrict"

showCvs :: Set CV -> String
showCvs = concatMap show . Set.toList

volatileQual :: Parser CV
volatileQual = Volatile <$ word "volatile"

--------------------------------------------------------------------------------
-- Unique tags
--
-- When compressing a symbol, we do a depth-first pre-order traversal of the
-- signature AST. We don't want to substitute a type with a reference to itself,
-- so we give each type a unique tag.

newtype Uniq = Uniq Int
  deriving (Enum, Eq, Show)

genUniq :: Parser Uniq
genUniq = do
  next <- getState
  modifyState succ
  return next

typeUniq :: Type -> Maybe Uniq
typeUniq Builtin{} = Nothing
typeUniq (Named _ _ u) = Just u
typeUniq (Ptr _ u) = Just u
typeUniq (Ref _ u) = Just u
typeUniq (CV _ _ u) = Just u

--------------------------------------------------------------------------------
-- Parser utilities
--
-- Parsec's user state is used to generate unique tags for types. See 'Uniq'.

type Parser a = ParsecT String Uniq Identity a

commas :: Parser a -> Parser [a]
commas = (`sepEndBy` word ",")

list :: Parser a -> Parser [a]
list = paren . commas

lexeme :: Parser String -> Parser String
lexeme = (<* spaces)

opt :: (Monoid a) => Parser a -> Parser a
opt = option mempty

paren :: Parser a -> Parser a
paren = between (word "(") (word ")")

word :: String -> Parser String
word = try . lexeme . string

--------------------------------------------------------------------------------
-- Builtins
--
-- Builtin types are mangled differently from user-defined types.

data Builtin
  = Void
  | WChar
  | Bool
  | Char
  | SChar
  | UChar
  | Short
  | UShort
  | Int
  | UInt
  | Long
  | ULong
  | LongLong
  | ULongLong
  | LongLongLong
  | ULongLongLong
  | Float
  | Double
  | LongDouble
  | LongLongDouble
  | Char32
  | Char16
  deriving (Eq)

instance Show Builtin where
  show = \case
    Void -> "v"
    WChar -> "w"
    Bool -> "b"
    Char -> "c"
    SChar -> "a"
    UChar -> "h"
    Short -> "s"
    UShort -> "t"
    Int -> "i"
    UInt -> "j"
    Long -> "l"
    ULong -> "m"
    LongLong -> "x"
    ULongLong -> "y"
    LongLongLong -> "n"
    ULongLongLong -> "o"
    Float -> "f"
    Double -> "d"
    LongDouble -> "e"
    LongLongDouble -> "g"
    Char32 -> "Di"
    Char16 -> "Ds"

maybeBuiltin :: [String] -> Maybe Builtin
maybeBuiltin = \case
  ["void"] -> Just Void
  ["wchar_t"] -> Just WChar
  ["bool"] -> Just Bool
  ["char"] -> Just Char
  -- WTB disjunctive patterns.
  ["signed", "char"] -> Just SChar
  ["int8_t"] -> Just SChar
  ["unsigned", "char"] -> Just UChar
  ["uint8_t"] -> Just UChar
  ["short"] -> Just Short
  ["short", "int"] -> Just Short
  ["int16_t"] -> Just Short
  ["unsigned", "short"] -> Just UShort
  ["unsigned", "short", "int"] -> Just UShort
  ["uint16_t"] -> Just UShort
  ["int"] -> Just Int
  ["int32_t"] -> Just Int
  ["unsigned"] -> Just UInt
  ["unsigned", "int"] -> Just UInt
  ["uint32_t"] -> Just UInt
  ["long"] -> Just Long
  ["int64_t"] -> Just Long
  ["unsigned", "long"] -> Just ULong
  ["unsigned", "long", "int"] -> Just ULong
  ["uint64_t"] -> Just ULong
  ["size_t"] -> Just ULong
  ["long", "long"] -> Just LongLong
  ["long", "long", "int"] -> Just LongLong
  ["__int64"] -> Just LongLong
  ["unsigned", "long", "long"] -> Just ULongLong
  ["unsigned", "long", "long", "int"] -> Just ULongLong
  ["unsigned", "__int64"] -> Just ULongLong
  ["__int128"] -> Just LongLongLong
  ["unsigned", "__int128"] -> Just ULongLongLong
  ["float"] -> Just Float
  ["double"] -> Just Double
  ["long", "double"] -> Just LongDouble
  ["__float80"] -> Just LongDouble
  ["__float128"] -> Just LongLongDouble
  ["char32_t"] -> Just Char32
  ["char16_t"] -> Just Char16
  _ -> Nothing
