-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

-- | Implementation of the Thrift Simple JSON Protocol
module Thrift.Protocol.JSON
  ( serializeJSON, serializePrettyJSON
  , deserializeJSON
  , JSON
  , keyToStr, hmMapKeys
  ) where

import Control.Arrow hiding (loop)
import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Aeson.Encoding as AE
import Thrift.Binary.Parser as P
import Data.ByteString (ByteString)
import Data.ByteString.Builder as B
import Data.ByteString.Builder.Prim as Prim
import Data.Function
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding
import Data.Word8
import GHC.Float
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap

import Thrift.Protocol
import Thrift.Protocol.JSON.Base64
import Thrift.Protocol.JSON.String

-- | Implementation of TSimpleJSONProtocol
serializeJSON :: ThriftSerializable a => a -> ByteString
serializeJSON = serializeGen (Proxy :: Proxy JSON)

-- | Pretty printed version of SimpleJSONProtocol
-- We still use Aeson here for now
serializePrettyJSON :: ThriftSerializable a => a -> ByteString
serializePrettyJSON = LBS.toStrict . encodePretty

deserializeJSON :: ThriftSerializable a => ByteString -> Either String a
deserializeJSON = deserializeGen (Proxy :: Proxy JSON)

-- Helper for converting Maps --------------------------------------------------

-- The keys of JSON Maps have to be Text, but in thrift they can be anything, so
-- we have these helpers to fix them up

keyToStr :: ToJSON k => k -> Text
keyToStr = decodeUtf8 . LBS.toStrict . encode

hmMapKeys :: (Eq b, Hashable b) => (a -> b) -> HashMap a x -> HashMap b x
hmMapKeys f = HashMap.fromList . map (first f) . HashMap.toList

-- BinaryProtocol instance -----------------------------------------------------

data JSON

#define COMMA 44
#define COLON 58
#define QUOTE 34
#define LCURLY 123
#define RCURLY 125
#define LBRACK 91
#define RBRACK 93

#define C_t 116
#define C_f 102
#define C_n 110

version1 :: Int
version1 = 1

-- These Type codes don't appear in the serialized data, rather we just make
-- them up in order to make sure that the value in the serialized data matches
-- what we expect. There are several types that we can't distinguish between
-- (eg numbers, lists/sets, etc)
numberTy, boolTy, strTy, listTy, structTy, nullTy :: Word8
numberTy = 0
boolTy   = 1
strTy    = 2
listTy   = 3
structTy = 4
nullTy   = 5

instance Protocol JSON where
  -- Generators for Types
  getByteType   _ = numberTy
  getI16Type    _ = numberTy
  getI32Type    _ = numberTy
  getI64Type    _ = numberTy
  getFloatType  _ = numberTy
  getDoubleType _ = numberTy
  getBoolType   _ = boolTy
  getStringType _ = strTy
  getListType   _ = listTy
  getSetType    _ = listTy
  getMapType    _ = structTy
  getStructType _ = structTy

  -- Generators for tokens
  genMsgBegin _ name msgTy seqNum =
    "[" <> B.intDec version1   <>
    "," <> genEscString name <>
    "," <> B.word8Dec msgTy    <>
    "," <> B.int32Dec seqNum   <>
    ","
  genMsgEnd _ = "]"

  genStruct _ fields = "{" <> mconcat (intersperse "," fields) <> "}"

  genField _ name _ _ _ val = genEscString name <> ":" <> val
  genFieldPrim _ name _ _ _ prim val =
    genEscString name <> ":" <> primBounded prim val

  genList _ _ build elems =
    "[" <> mconcat (intersperse "," (map build elems)) <> "]"

  genListPrim p ty prim = genList p ty (primBounded prim)

  genMap _ _ _ isString kbuild vbuild elems =
    "{" <> mconcat (intersperse "," (map build elems)) <> "}"
    where
      build (k, v)
        | isString  = kbuild k <> ":" <> vbuild v
        | otherwise = "\"" <> kbuild k <> "\":" <> vbuild v

  genMapPrim p k v b kb vb = genMap p k v b (primBounded kb) (primBounded vb)

  -- Generators for base types
  genBytePrim _ = Prim.int8Dec
  genI16Prim  _ = Prim.int16Dec
  genI32Prim  _ = Prim.int32Dec
  genI64Prim  _ = Prim.int64Dec
  genFloat _ num
    | isNaN num = Prim.primMapListFixed Prim.char7 "\"NaN\""
    -- Generate x rather than x.0, like C++'s serializer
    | num == fromInteger (round num) = B.intDec $ round num
    | otherwise = floatDec num
  genDouble _ num
    | isNaN num = Prim.primMapListFixed Prim.char7 "\"NaN\""
    -- Generate x rather than x.0, like C++'s serializer
    | num == fromInteger (round num) = B.intDec $ round num
    | otherwise = doubleDec num
  genBoolPrim _ = (condB id `on` liftFixedToBounded)
    (const ('t', ('r', ('u', 'e'))) >$<
     (Prim.char7 >*< Prim.char7 >*< Prim.char7 >*< Prim.char7))
    (const ('f', ('a', ('l', ('s', 'e')))) >$<
     (Prim.char7 >*< Prim.char7 >*< Prim.char7 >*< Prim.char7 >*< Prim.char7))

  genText _ = genEscString
  genBytes _ bs = "\"" <> encodeBase64 bs <> "\""

  -- Parsers for tokens
  parseMsgBegin _ = do
    _       <- skipSpaces *> P.word8 LBRACK
    version <- skipSpaces *> parseJSONInt
    when (version /= version1) $ fail "parseMsgBegin: invalid version"
    _       <- skipSpaces *> P.word8 COMMA
    name    <- parseJSONString
    _       <- skipSpaces *> P.word8 COMMA
    msgType <- parseJSONInt
    _       <- skipSpaces *> P.word8 COMMA
    seqNum  <- parseJSONInt
    _       <- skipSpaces *> P.word8 COMMA
    return $ MsgBegin name msgType seqNum
  parseMsgEnd _ = void $ skipSpaces *> P.word8 RBRACK

  parseFieldBegin _ lastId idMap = do
    w <- skipSpaces *> anyWord8
    case w of
      -- If we get a '{' then the struct might be empty
      LCURLY | lastId == 0 -> do
        c <- skipSpaces *> P.peek
        case c of
          RCURLY -> FieldEnd <$ anyWord8
          _ -> go
      COMMA -> go
      RCURLY -> pure FieldEnd
      _  -> fail $ "no field " ++ show w ++ " " ++ show lastId
    where
      go = do
        name <- parseJSONString
        _    <- skipSpaces *> P.word8 COLON
        case HashMap.lookup name idMap of
          Just fid -> do
            c <- skipSpaces *> P.peek
            let
              ty = case c of
                LCURLY -> structTy
                LBRACK -> listTy
                QUOTE  -> strTy
                C_f    -> boolTy
                C_t    -> boolTy
                C_n    -> nullTy
                _      -> numberTy
            pure $ FieldBegin ty fid False
          -- Inserting a 0xFF type code ensures that this won't match with
          -- anything
          Nothing  -> pure $ FieldBegin 0xFF (-1) False

  -- Parser for base types
  parseByte   _ = parseJSONInt
  parseI16    _ = parseJSONInt
  parseI32    _ = parseJSONInt
  parseI64    _ = parseJSONInt
  parseFloat  _ = double2Float <$> parseJSONDouble
  parseDouble _ = parseJSONDouble
  parseBool   _ = parseJSONBool
  parseBoolF _ _ = parseJSONBool
  parseText _ = parseJSONString
  parseBytes _ = do
    raw <- P.word8 QUOTE *> P.takeWhile (/= QUOTE) <* P.word8 QUOTE
    return $ decodeBase64 raw

  parseList _ p = do
    _ <- skipSpaces *> P.word8 LBRACK
    w <- skipSpaces *> P.peek
    if w == RBRACK then anyWord8 *> pure (0,[]) else loop 1 []
      where
        loop !n xs = do
          x <- skipSpaces *> p
          w <- skipSpaces *> anyWord8
          case w of
            RBRACK -> pure (n, reverse (x:xs))
            COMMA  -> loop (n+1) (x:xs)
            _ -> fail "invalid list"

  parseMap _ pk pv isString = do
    _ <- skipSpaces *> P.word8 LCURLY
    w <- skipSpaces *> P.peek
    if w == RCURLY then anyWord8 *> pure [] else loop
      where
        loop = do
          skipSpaces
          key <- if isString then pk else P.word8 QUOTE *> pk <* P.word8 QUOTE
          _   <- skipSpaces *> P.word8 COLON
          val <- skipSpaces *> pv
          w <- skipSpaces *> anyWord8
          case w of
            RCURLY -> pure [(key, val)]
            COMMA  -> ((key, val) :) <$> loop
            _ -> fail "invalid map"

  parseSkip _ _ _ = do
    c <- skipSpaces *> P.peek
    case c of
      LCURLY -> skipObj
      LBRACK -> skipList
      QUOTE  -> void $ parseJSONString
      C_f    -> void $ string "false"
      C_t    -> void $ string "true"
      C_n    -> void $ string "null"
      _ | c >= _0 && c <= _9 || c == _hyphen -> void double
        | otherwise -> fail "not a valid json value"

skipObj :: Parser ()
skipObj = do
  _ <- P.word8 LCURLY
  _ <- sepBy skipInner $ skipSpaces *> P.word8 COMMA
  _ <- skipSpaces *> P.word8 RCURLY
  return ()
  where
    skipInner = do
      _ <- skipSpaces *> parseSkip (Proxy @JSON) 1 Nothing
      _ <- skipSpaces *> P.word8 COLON
      skipSpaces *> parseSkip (Proxy @JSON) 1 Nothing

skipList :: Parser ()
skipList = do
  _ <- P.word8 LBRACK
  _ <- sepBy skipInner $ skipSpaces *> P.word8 COMMA
  _ <- skipSpaces *> P.word8 RBRACK
  return ()
  where
    skipInner = skipSpaces *> parseSkip (Proxy @JSON) 1 Nothing

-- Use Aeson to escape a string
genEscString :: Text -> B.Builder
genEscString = fromEncoding . AE.text

{-# INLINE parseJSONInt #-}
parseJSONInt :: Integral a => Parser a
parseJSONInt = lexeme $ signed decimal

{-# INLINE parseJSONBool #-}
parseJSONBool :: Parser Bool
parseJSONBool = do
  skipSpaces
  w <- anyWord8
  if | w == C_t -> True <$ string "rue"
     | w == C_f -> False <$ string "alse"
     | otherwise -> fail "expected boolean"

{-# INLINE parseJSONDouble #-}
parseJSONDouble :: Parser Double
parseJSONDouble = do
  c <- skipSpaces *> P.peek
  if | c == QUOTE -> string "\"NaN\"" >> return (0/0)
     | otherwise -> double


{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme = (skipSpaces *>)
