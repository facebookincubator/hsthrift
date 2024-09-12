{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- FromJSON instance below is an orphan (deliberately)
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}

module Util.Aeson
  ( toJSONText
  , toJSONByteString
  , cStringToObject
  , unsafeCStringToObject
  , unsafeCStringLenToObject
  , prettyJSON
  , parseValueStrict'
  , parseValueStrict
    -- * Backwards compatibility
  , ObjectKey
  , KeyMap
  , keyToText
  , keysToTexts
  , keyFromText
  , objectToList
  , objectFromList
  , objectKeys
  , objectToHashMap
  , objectFromHashMap
  , objectToHashMapText
  , objectFromHashMapText
  , emptyKeyMap
  , insertKeyMap
  , unionKeyMap
  , keyMapSize
  , lookupKeyMap
  ) where

import Data.Aeson
import Data.Aeson.Parser (value, value')
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Coerce
import Data.Type.Coercion
#endif
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
        [ sep [ pp (String (keyToText key)) <+> char ':', nest 2 (pp v) ]
        | (key, v) <- HashMap.toList (objectToHashMap hm) ]
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

-- Orphan instance to make from/toJSON O(1) when converting from/to an Object
-- (aka HashMap Text Value).  Otherwise the default instances provided
-- by Aeson will rebuild the HashMap.
--
-- This needs to be not just OVERLAPPABLE, but INCOHERENT, because
-- otherwise a constraint "FromJSON (HashMap Text a)" doesn't have a
-- single most-specific instance to resolve to.  INCOHERENT is ok;
-- the worst that can happen is that we lose the optimisation.
instance {-# INCOHERENT #-} FromJSON Object where
  parseJSON (Object obj) = return obj
  parseJSON other = typeMismatch "object" other

-- Having same optimization for 'ToJSON' is still valuable when we cannot use
-- 'Object' directly, e.g., when we are calling a polymorphic function with
-- a @ToJSON a@ constraint.
instance {-# INCOHERENT #-} ToJSON Object where
  toJSON = Object

keyToText :: ObjectKey -> Text
keysToTexts :: [ObjectKey] -> [Text]  -- zero-cost coercion
keyFromText :: Text -> ObjectKey
objectToList :: KeyMap v -> [(ObjectKey, v)]
objectFromList :: [(ObjectKey, v)] -> KeyMap v
objectKeys :: KeyMap v -> [ObjectKey]
objectToHashMap :: KeyMap v -> HashMap ObjectKey v
objectFromHashMap :: HashMap ObjectKey Value -> Object
objectToHashMapText :: Object -> HashMap Text Value
objectFromHashMapText :: HashMap Text Value -> Object
emptyKeyMap :: KeyMap v
insertKeyMap :: ObjectKey -> v -> KeyMap v -> KeyMap v
unionKeyMap :: KeyMap v -> KeyMap v -> KeyMap v
keyMapSize :: KeyMap v -> Int
lookupKeyMap :: ObjectKey -> KeyMap v -> Maybe v

#if MIN_VERSION_aeson(2,0,0)
type ObjectKey = Key
type KeyMap = KeyMap.KeyMap
keyToText = toText
keysToTexts = case Key.coercionToText of
  Just Coercion -> coerce
  _ -> map keyToText
keyFromText = fromText
objectToList = KeyMap.toList
objectFromList = KeyMap.fromList
objectKeys = KeyMap.keys
objectToHashMap = KeyMap.toHashMap
objectFromHashMap = KeyMap.fromHashMap
objectToHashMapText = KeyMap.toHashMapText
objectFromHashMapText = KeyMap.fromHashMapText
emptyKeyMap = KeyMap.empty
insertKeyMap = KeyMap.insert
unionKeyMap = KeyMap.union
keyMapSize = KeyMap.size
lookupKeyMap = KeyMap.lookup
#else
type ObjectKey = Text
type KeyMap v = HashMap Text v
keyToText = id
keysToTexts = id
keyFromText = id
objectToList = HashMap.toList
objectFromList = HashMap.fromList
objectKeys = HashMap.keys
objectToHashMap = id
objectFromHashMap = id
objectToHashMapText = id
objectFromHashMapText = id
emptyKeyMap = HashMap.empty
insertKeyMap = HashMap.insert
unionKeyMap = HashMap.union
keyMapSize = HashMap.size
lookupKeyMap = HashMap.lookup
#endif
