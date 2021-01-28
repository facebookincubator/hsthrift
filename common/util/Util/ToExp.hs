-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.ToExp
  ( ToExp(..)
  , intE
  , fracE
  , con
  , recConstr
  , fun
  , (=.=)
  , (=.==)
  , (=.=?)
  , (=$=)
  , (=$==)
  , pp
  , ppShow
  ) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as SVector
import Language.Haskell.Exts hiding (intE)
import Util.Text

infixl 1 =.=
infixl 1 =.==
infixl 1 =.=?
infixl 2 =$=
infixl 2 =$==

intE :: Integer -> Exp ()
intE n = (if n < 0 then paren else id) $
           Lit () (Int () n (show n))

fracE :: Rational -> Exp ()
fracE n = (if n < 0 then paren else id) $
            Lit () (Frac () n (show n))

con :: String -> Exp ()
con = Con () . UnQual () . name

recConstr :: String -> [FieldUpdate ()] -> Exp ()
recConstr recname = RecConstr () (UnQual () $ name recname)

fun :: String -> Exp ()
fun = function

-- | Function used to generate FieldUpdate for record constructors.
(=.=) :: ToExp a => String -> a -> FieldUpdate ()
field =.= value = FieldUpdate () (UnQual () $ name field) (toExp value)

(=.==) :: String -> Exp () -> FieldUpdate ()
(=.==) = (=.=)

(=.=?) :: String -> Maybe (Exp ()) -> FieldUpdate ()
(=.=?) = (=.=)

(=$=) :: ToExp a => Exp () -> a -> Exp ()
f =$= x = app f (toExp x)

(=$==) :: Exp () -> Exp () -> Exp ()
(=$==) = (=$=)

class ToExp a where
  toExp :: a -> Exp ()

instance ToExp (Exp ()) where
  toExp = id

instance ToExp () where
  toExp () = tuple []

instance ToExp Bool where
  toExp = con . show

instance ToExp Int where
  toExp = intE . fromIntegral

instance ToExp Double where
  toExp = fracE . realToFrac

instance ToExp Text where
  toExp = strE . Text.unpack

instance ToExp TextAndByteString where
  toExp s = fun "mkTextAndByteString" =$= toByteString s

instance ToExp BS.ByteString where
  toExp = strE . BS.unpack

instance (ToExp a, ToExp b) => ToExp (a, b) where
  toExp (a, b) = tuple [toExp a, toExp b]

instance ToExp a => ToExp (Maybe a) where
  toExp = maybe (con "Nothing") (con "Just" =$=)

instance ToExp a => ToExp [a] where
  toExp = listE . map toExp

instance ToExp Int64 where
  toExp = intE . fromIntegral

instance ToExp Char where
  toExp = charE


instance ToExp a => ToExp (Vector.Vector a) where
  toExp v = fun "Vector.fromList" =$= Vector.toList v

instance (SVector.Storable a, ToExp a)
    => ToExp (SVector.Vector a) where
  toExp v = fun "SVector.fromList" =$= SVector.toList v

instance (ToExp k, ToExp v) => ToExp (HashMap k v) where
  toExp m = fun "HashMap.fromList" =$= HashMap.toList m

instance ToExp A.Value where
  toExp A.Null = con "Null"
  toExp (A.Bool b) = con "Bool" =$= b
  toExp (A.Number n) = con "Number" =$==
    either toExp toExp (floatingOrInteger n :: Either Double Int)
  toExp (A.String s) = con "String" =$= s
  toExp (A.Array a) = con "Array" =$= a
  toExp (A.Object o) = con "Object" =$= o

-- TODO(t16157798): remove the OneLineMode; currently dumpCacheAsHaskellFn
-- depends on the output being a single line only
pp :: ToExp a => a -> String
pp x = prettyPrintStyleMode oneline defaultMode $ (noLoc <$ toExp x)
  where
  oneline = Style{ mode = OneLineMode, lineLength = 0, ribbonsPerLine = 0 }

-- To define a Show instance based on ToExp, use ppShow instead of pp;
-- the correct definition would define showsPrec and depending on the
-- operator precedence of the enclosing context and on the precedence inside
-- the expression would choose to parenthesize it or not.
-- YOLO. Let's just always parenthesize to be on the safe side.
--
-- > instance Show Foo where
-- >  show = ppShow
ppShow :: ToExp a => a -> String
ppShow x = pp $ paren (toExp x)
