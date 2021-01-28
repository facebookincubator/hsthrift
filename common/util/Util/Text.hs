-- Copyright (c) Facebook, Inc. and its affiliates.

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE CPP #-}

module Util.Text
  ( cStringLenToText
  , cStringLenToTextAndByteString
  , cStringLenToTextLenient
  , cStringToText
  , cStringToTextLenient
  , newCStringFromText
  , useMaybeTextAsCString
  , useTextAsCString
  , useTextAsCStringLen
  , useTextsAsCStrings
  , useTextsAsCStringLens
  , withCStrings
  , textShow
  , TextRead(..)
  , TextAndByteString(..)
  , TextOrByteString(..)
  , mkTextAndByteString
  , nl
  , capitalize
  , decapitalize
  , insertCommasAndAnd
  , toCamelCase
  , toPascalCase
  , toText
  , toUnderscore
  , isCfInfixOf
  , UnicodeException
  , Util.Text.withCStringLen
  , useAsPtr
  , InvalidConversion(..)
  , textToInt
  , hexToInt
  , wrapText
  ) where

import Control.Arrow (first)
import Control.Exception (Exception, throw)
import qualified Data.Text.Array as A
import Data.Binary
import Data.ByteString (ByteString, useAsCStringLen)
import qualified Data.ByteString as BS
import Data.Char
import Data.Function (on)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HashMap
import Data.String
import Data.Text.Encoding (encodeUtf8, decodeUtf8, decodeUtf8', decodeUtf8With)
import Data.Text.Encoding.Error (UnicodeException, lenientDecode)
import Data.Typeable
import Foreign
import Foreign.C
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text as Text
import Data.Text.Internal
import qualified Data.Text.Foreign as Text

import TextShow hiding (toText)
import Util.ByteString
import qualified Util.ASan as ASan

-- | Executes an 'IO' action with an array of 'CString's marshalled from
-- 'String'.
withCStrings :: [String] -> (Ptr CString -> IO a) -> IO a
withCStrings strs f = go [] strs
  where go cs (t:ts) = withCString t $ \c -> go (c:cs) ts
        go cs [] = withArray0 nullPtr (reverse cs) f

-- | Converts a null-terminated 'CString' to 'Text'.
cStringToText :: CString -> IO Text
cStringToText s = do
  bs <- B.unsafePackCString s
  return $! decodeUtf8 bs


cStringToTextLenient :: CString -> IO Text
cStringToTextLenient s = do
  bs <- B.unsafePackCString s
  return $! decodeUtf8With lenientDecode bs

-- | Converts a 'CString' with known length to 'Text'.
cStringLenToText :: CString -> Int -> IO Text
cStringLenToText s len = do
  bs <- B.unsafePackCStringLen (s, len)
  return $! decodeUtf8 bs

cStringLenToTextAndByteString :: CString -> Int -> IO TextAndByteString
cStringLenToTextAndByteString s len = do
  bs <- Char8.packCStringLen (s, len)
  return $! mkTextAndByteString bs

cStringLenToTextLenient :: CString -> Int -> IO Text
cStringLenToTextLenient s len = do
  bs <- B.unsafePackCStringLen (s, len)
  return $! decodeUtf8With lenientDecode bs

-- | Executes an 'IO' action with a 'CString' marshalled from 'Just'
-- 'Text', or a 'nullPtr' from 'Nothing'.
useMaybeTextAsCString :: Maybe Text -> (CString -> IO a) -> IO a
useMaybeTextAsCString (Just text) m = useTextAsCString text m
useMaybeTextAsCString Nothing m = m nullPtr

-- | Executes an 'IO' action with a 'Text' value marshalled as a UTF-8
-- 'CString'.
useTextAsCString :: Text -> (CString -> IO a) -> IO a
-- TODO(T24195918): We need to do this for ABI compatibility between
-- sigma.service and sigma.service.asan. Codemod to __SANITIZE_ADDRESS__
-- to enable ASAN checks for Haskell allocations.
#ifdef __HASKELL_SANITIZE_ADDRESS__
useTextAsCString = ASan.textWithCString
#else
useTextAsCString = BS.useAsCString . encodeUtf8
#endif

-- | Executes an 'IO' action with a 'Text' value marshalled as a UTF-8
-- 'CStringLen'.
useTextAsCStringLen :: Text -> (CStringLen -> IO a) -> IO a
#ifdef __HASKELL_SANITIZE_ADDRESS__
useTextAsCStringLen = ASan.textWithCStringLen
#else
useTextAsCStringLen = BS.useAsCStringLen . encodeUtf8
#endif

-- | Executes an 'IO' action with a '[Text]' value marshalled as a UTF-8
-- 'Ptr CStringLen'.
useTextsAsCStringLens :: [Text]
                      -> (Ptr CString -> Ptr CSize -> CSize -> IO a)
                      -> IO a
useTextsAsCStringLens = bsListAsCStrLenArr . map encodeUtf8

{-# INLINE withCStringLen #-}
withCStringLen :: Text -> (CStringLen -> IO a) -> IO a
#ifdef __HASKELL_SANITIZE_ADDRESS__
withCStringLen = ASan.textWithCStringLen
#else
withCStringLen = Text.withCStringLen
#endif

-- | Executes an 'IO' action with an array of 'CStrings' marshalled from
-- 'Text'.
useTextsAsCStrings :: [Text] -> (Ptr CString -> IO a) -> IO a
useTextsAsCStrings texts f = go [] texts
  where
  go cs (t:ts) =
    Util.ByteString.useAsCString (encodeUtf8 t) $ \c -> go (c:cs) ts
  go cs [] = withArray0 nullPtr (reverse cs) f

-- | Converts a 'Text' to a 'CString'. The resulting 'CString' must be
-- 'free'd.
newCStringFromText :: Text -> IO CString
newCStringFromText = newCStringFromLazyByteString
  . B.fromChunks . (:[]) . encodeUtf8

-- | A convenience wrapper for 'Show'.
textShow :: Show a => a -> Text
textShow = Text.pack . show

{-# INLINE useAsPtr #-}
useAsPtr :: Text -> (Ptr Word16 -> Text.I16 -> IO a) -> IO a
#ifdef __HASKELL_SANITIZE_ADDRESS__
useAsPtr = ASan.textUseAsPtr
#else
useAsPtr = Text.useAsPtr
#endif

-- | A pair of a 'Text' and a 'ByteString'. Useful for when we get a
-- value in one format and we want to lazily compute and cache the
-- conversion to the other format.
--
-- /Note:/ if/when conversion is free (i.e. 'Text' is stored as UTF-8)
-- then we can get rid of this.

data TextAndByteString = TextAndByteString
  { toTextOrError :: Either UnicodeException Text -- deliberately lazy
  , toByteString :: ByteString }
  deriving Typeable

toText :: TextAndByteString -> Text
toText v
  = case toTextOrError v of
      Left e -> throw e
      Right v -> v

instance Eq TextAndByteString where
  a == b = toByteString a == toByteString b

instance Binary TextAndByteString where
  put = put . toByteString
  get = mkTextAndByteString <$> get

-- We don't need to show both fields, and indeed the contents might
-- not be valid Text, so the Show instance must show only the
-- ByteString component.
instance Show TextAndByteString where
  showsPrec p tbs =
    showParen (p > 10)
    $ showString "mkTextAndByteString "
    . shows (Char8.unpack (fromTextAndByteString tbs))
    -- the showParen is necessary so that instead of e.g.
    --    Foo mkTextAndByteString ""
    -- we get
    --    Foo (mkTextAndByteString "")

instance Hashable TextAndByteString where
  hashWithSalt s = hashWithSalt s . toByteString

-- | Type-specialised constructor
mkTextAndByteString :: ByteString -> TextAndByteString
mkTextAndByteString = toTextAndByteString

instance IsString TextAndByteString where
  fromString xs = toTextAndByteString (Text.pack xs)

class TextOrByteString a where
  fromTextAndByteString :: TextAndByteString -> a
  toTextAndByteString :: a -> TextAndByteString

instance TextOrByteString Text where
  fromTextAndByteString = toText
  toTextAndByteString txt = TextAndByteString {
    toByteString = encodeUtf8 txt,
    toTextOrError = Right txt
  }

instance TextOrByteString ByteString where
  fromTextAndByteString = toByteString
  toTextAndByteString str = TextAndByteString {
    toByteString = str,
    toTextOrError = decodeUtf8' str
  }


-- | Create a number of newlines.
nl :: Int -> Text
nl = flip Text.replicate "\n"

-- | Capitalize the first non-whitespace character of a Text string.
capitalize :: Text -> Text
capitalize s = ws <> c <> cs
  where
    (ws, s') = Text.span isSpace s
    (c, cs) = first Text.toUpper . Text.splitAt 1 $ s'

-- | Lowercase the first non-whitespace character of a Text string.
decapitalize :: Text -> Text
decapitalize s =
  case Text.uncons t of
    Just (c, cs) | c /= toLower c -> ws <> Text.cons (toLower c) cs
    _ -> s
  where
    (ws, t) = Text.span isSpace s

toCamelCase :: Text -> Text
toCamelCase = decapitalize . toPascalCase

toPascalCase :: Text -> Text
toPascalCase t = Text.concat $ map capitalize (Text.splitOn "_" t)

toUnderscore :: Text -> Text
toUnderscore t = Text.intercalate "_" $
  map Text.toLower $ Text.groupBy (\_ i -> not $ isUpper i) t

-- | Search for substring whilst ignoring case.
isCfInfixOf :: Text -> Text -> Bool
isCfInfixOf = Text.isInfixOf `on` Text.toCaseFold

class TextRead a where
  readText :: Text -> Maybe a

  -- Default implementation for enumerations
  default readText :: (Bounded a, Enum a, TextShow a) => Text -> Maybe a
  readText =
    let
      m = HashMap.fromList [ (showt v, v) | v <- [minBound..maxBound] ]
    in \t -> HashMap.lookup t m
  {-# INLINE readText #-}

newtype InvalidConversion = InvalidConversion Text
  deriving (Typeable, Eq, Show)

instance Exception InvalidConversion

-- | Attempts to convert a decimal representation of an integer to an
-- 'Int'.  Returns 'Left' with an error message if the input 'Text'
-- was not a valid integer, or 'Right' with the value otherwise.
textToInt :: Text -> Either InvalidConversion Int
textToInt txt@(Text arr off len)
  | len == 0 = err
  | fromIntegral (A.unsafeIndex arr off) == ord '-' =
    if len > 1
       then negate <$> go 0 (off+1)
       else err
  | otherwise = go 0 off
  where
  -- Use Text internals for speed.  We know that digits are never in
  -- the upper range of UTF-16.
  go !n !i
    | i == len + off = Right n
    | c >= 0 && c <= 9 = go (n * 10 + c) (i + 1)
    | otherwise = err
    where
      c = fromIntegral (A.unsafeIndex arr i) - ord '0'

  err = Left . InvalidConversion $
    "textToInt: string \"" <> txt <> "\" is not an integer"

-- | Attempts to convert a hexadecimal representation of an integer to an
-- 'Int'.  Returns 'Left' with an error message if the input 'Text'
-- was not a valid integer, or 'Right' with the value otherwise.
hexToInt :: Text -> Either InvalidConversion Int
hexToInt txt@(Text arr off len)
  | len == 0 = err
  | fromIntegral (A.unsafeIndex arr off) == ord '-' =
    if len > 1
       then negate <$> go 0 (off+1)
       else err
  | otherwise = go 0 off
  where
  -- Use Text internals for speed.  We know that digits are never in
  -- the upper range of UTF-16.
  go !n !i
    | i == len + off = Right n
    | c >= ord '0' && c <= ord '9' = go (n * 16 + c - ord '0') (i + 1)
    | c >= ord 'a' && c <= ord 'f' = go (n * 16 + c - ord 'a' + 10) (i + 1)
    | c >= ord 'A' && c <= ord 'F' = go (n * 16 + c - ord 'A' + 10) (i + 1)
    | otherwise = err
    where
      c = fromIntegral (A.unsafeIndex arr i)

  err = Left $ InvalidConversion $
          "hexToInt: string \"" <> txt <> "\" is not an integer"

insertCommasAndAnd :: [Text] -> Text
insertCommasAndAnd [] = ""
insertCommasAndAnd [x] = x
insertCommasAndAnd [x, y] = x <> " and " <> y
insertCommasAndAnd [x, y, z] = x <> ", " <> y <> ", and " <> z
insertCommasAndAnd (x : xs) = x <> ", " <> insertCommasAndAnd xs

-- | Break a Text into lines such that each one fits within the given number of
-- characters.
-- NB: this function does not preserve existing whitespace formatting.
wrapText
  :: Int  -- ^ Indent Amount
  -> Int  -- ^ Max characters per line
  -> Text -- ^ Text to wrap
  -> [Text]
wrapText indent maxLength text = concatMap wrapLine $ Text.lines text
  where
    wrapLine line = case Text.words line of
      [] -> []
      [word] -> [word]
      w:ws -> wrap (margin <> w) (indent + Text.length w) ws

    wrap acc _ [] = [acc]
    wrap acc n (w:ws)
      -- If adding the word exceeds the limit, then wrap
      | len > maxLength = acc : wrap (margin <> w) (indent + Text.length w) ws
      -- Otherwise, add to the current line. Not that there is no length check
      -- here. If the word is longer than the max line length then we will put
      -- it in anyway.
      | otherwise = wrap (acc <> " " <> w) len ws
      where
        len = n + Text.length w + 1
    margin = Text.replicate indent " "
