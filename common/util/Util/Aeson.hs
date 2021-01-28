-- Copyright (c) Facebook, Inc. and its affiliates.

-- FromJSON instance below is an orphan (deliberately)
{-# OPTIONS_GHC -Wno-orphans #-}

module Util.Aeson
  ( toJSONText
  , toJSONByteString
  , cStringToObject
  , unsafeCStringToObject
  , unsafeCStringLenToObject
  , prettyJSON
  , parseValueStrict'
  , parseValueStrict
  ) where

import Data.Aeson
import Data.Aeson.Parser (value, value')
import Data.Aeson.Types (typeMismatch)
import Data.ByteString (ByteString)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as LB
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Foreign.C

import Text.PrettyPrint
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Converts a value to JSON, and returns it as a 'Text'
toJSONText :: ToJSON x => x -> Text.Text
toJSONText = Text.decodeUtf8 . toJSONByteString

-- | Converts a value to JSON, and returns it as a 'ByteString'
toJSONByteString :: ToJSON x => x -> ByteString
toJSONByteString = LB.toStrict . encode

-- | Since Aeson is lazily parsed, this assumes that that the memory
-- pointed to in the 'CString' will live throughout the liftime of the
-- returned object. If not, this potentially references freed memory. If
-- you cannot guarantee the lifetime, use 'cStringToObject'.
unsafeCStringToObject :: CString -> IO (Maybe Object)
unsafeCStringToObject s = decodeStrict <$> B.unsafePackCString s

-- | like 'unsafeCStringToObject' but takes a length too
unsafeCStringLenToObject :: CString -> Int -> IO (Maybe Object)
unsafeCStringLenToObject s l = decodeStrict <$> B.unsafePackCStringLen (s,l)

-- | Copies the 'CString' contents into Haskell, and then creates a
-- lazily parsed Aeson 'Object'.
cStringToObject :: CString -> IO (Maybe Object)
cStringToObject s = decodeStrict <$> B.packCString s

-- | Convert a JSON value to a 'String' with indentation to make it
-- easier to read.
prettyJSON :: Value -> String
prettyJSON val = show (pp val)
 where
  pp :: Value -> Doc
  pp Null = text "null"
  pp (Bool b) = text $ if b then "true" else "false"
  pp (String txt) =
    text (Text.unpack $ Text.decodeUtf8 $ LB.toStrict $ encode (String txt))
  pp (Number i) = text (show i)
  pp (Object hm) =
    braces $ sep $
      punctuate (char ',')
        [ sep [ pp (String key) <+> char ':', nest 2 (pp v) ]
        | (key, v) <- HashMap.toList hm ]
  pp (Array arr) =
    brackets $ sep $ punctuate (char ',') $ map pp $ Vector.toList arr


-- eitherDecode family of function  strictly requires that the Value is
-- either an object or an array.
-- This relaxes that restriction and allows other kinds of Values.
-- NOTE: this is fixed in latest aeson
parseValueStrict :: ByteString -> Either String Value
parseValueStrict = A.parseOnly
  (A.skipSpace *> value <* A.skipSpace <* A.endOfInput)

parseValueStrict' :: ByteString -> Either String Value
parseValueStrict' = A.parseOnly
  (A.skipSpace *> value' <* A.skipSpace <* A.endOfInput)

-- Orphan instance to make fromJSON O(1) when converting to an Object
-- (aka HashMap Text Value).  Otherwise the default instances provided
-- by Aeson will rebuild the HashMap.
--
-- This needs to be not just OVERLAPPABLE, but INCOHERENT, because
-- otherwise a constraint "FromJSON (HashMap Text a)" doesn't have a
-- single most-specific instance to resolve to.  INCOHERENT is ok;
-- the worst that can happen is that we lose the optimisation.
instance {-# INCOHERENT #-} FromJSON (HashMap Text Value) where
  parseJSON (Object obj) = return obj
  parseJSON other = typeMismatch "object" other
