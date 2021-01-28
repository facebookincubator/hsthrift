-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ConstraintKinds, CPP #-}
module Thrift.Protocol
  ( ThriftSerializable, serializeGen, deserializeGen
  , ThriftStruct(..), ProtocolException(..)
  , Protocol(..), parseStop, parseEnum, parseEnumNoUnknown
  , FieldId, Length, Type, MsgType, SeqNum
  , FieldBegin(..), MsgBegin(..)
  , genByte, genI16, genI32, genI64, genBool
  , ThriftEnum(..)
  , unknownThriftEnumErrorMsg
  ) where

import Control.Exception
import Data.Aeson hiding (Bool, String)
import Thrift.Binary.Parser
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.ByteString.Builder.Prim
import Data.HashMap.Strict as HashMap
import Data.Int
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding
import Data.Typeable
import Data.Word
import qualified Data.ByteString.Lazy as LBS

#if __GLASGOW_HASKELL__ == 806
import Prelude hiding (fail)
import Control.Monad.Fail (fail)
#endif


-- Constraint Kind that every Thrift generated datatype must satisfy
type ThriftSerializable a = (ToJSON a, ThriftStruct a)

class ThriftStruct a where
  buildStruct :: Protocol p => Proxy p -> a -> Builder
  parseStruct :: Protocol p => Proxy p -> Parser a

class ThriftEnum a where
  toThriftEnum        :: Int -> a
  fromThriftEnum      :: a -> Int
  allThriftEnumValues :: [a]
  toThriftEnumEither  :: Int -> Either String a

instance ThriftEnum Bool where
  toThriftEnum n | n == 0    = False
                 | n == 1    = True
                 | otherwise = errorWithoutStackTrace
                   "Thrift.Protocol.ThriftEnum.Bool.toThriftEnum: bad argument"
  fromThriftEnum False = 0
  fromThriftEnum True  = 1
  allThriftEnumValues = [False, True]
  toThriftEnumEither n | n == 0    = Right False
                       | n == 1    = Right True
                       | otherwise = Left "Thrift.Protocol.ThriftEnum.Bool.toThriftEnumEither: bad argument"

{-# INLINE serializeGen #-}
serializeGen
  :: (Protocol p, ThriftSerializable a)
  => Proxy p
  -> a
  -> ByteString
serializeGen proxy = LBS.toStrict . toLazyByteString . buildStruct proxy

{-# INLINE deserializeGen #-}
deserializeGen
  :: (Protocol p, ThriftSerializable a)
  => Proxy p
  -> ByteString
  -> Either String a
deserializeGen proxy input = parse (parseStruct proxy) input

newtype ProtocolException = ProtocolException String
  deriving (Show, Eq)
instance Exception ProtocolException

-- Binary Protocol type class --------------------------------------------------

type FieldId = Int16
type Length  = Word32
type Type    = Word8
type IdMap   = HashMap Text FieldId

type MsgType = Word8
type SeqNum  = Int32

data FieldBegin
  = FieldBegin {-# UNPACK #-} !Type {-# UNPACK #-} !FieldId !Bool
  | FieldEnd

data MsgBegin
  = MsgBegin !Text {-# UNPACK #-} !MsgType {-# UNPACK #-} !SeqNum
    deriving (Show, Eq)

class Protocol p where
  -- Generators for Types
  getByteType   :: Proxy p -> Type
  getI16Type    :: Proxy p -> Type
  getI32Type    :: Proxy p -> Type
  getI64Type    :: Proxy p -> Type
  getFloatType  :: Proxy p -> Type
  getDoubleType :: Proxy p -> Type
  getBoolType   :: Proxy p -> Type
  getStringType :: Proxy p -> Type
  getListType   :: Proxy p -> Type
  getSetType    :: Proxy p -> Type
  getMapType    :: Proxy p -> Type
  getStructType :: Proxy p -> Type

  -- Generators for tokens
  genMsgBegin   :: Proxy p -> Text -> MsgType -> SeqNum -> Builder

  genStruct  :: Proxy p -> [Builder] -> Builder

  genField
    :: Proxy p -> Text -> Type -> FieldId -> FieldId -> Builder -> Builder
  genFieldPrim
    :: Proxy p -> Text -> Type -> FieldId -> FieldId
    -> BoundedPrim a -> a -> Builder

  -- Only Compact protocol needs a custom implementation for this
  genFieldBool
    :: Proxy p -> Text -> FieldId -> FieldId -> Bool -> Builder
  genFieldBool proxy name fid lastId =
    genFieldPrim proxy name (getBoolType proxy) fid lastId (genBoolPrim proxy)

  genList :: Proxy p -> Type -> (a -> Builder) -> [a] -> Builder
  genListPrim :: Proxy p -> Type -> BoundedPrim a -> [a] -> Builder

  genMap  :: Proxy p -> Type -> Type -> Bool
          -> (k -> Builder) -> (v -> Builder)
          -> [(k, v)] -> Builder
  genMapPrim :: Proxy p -> Type -> Type -> Bool
             -> BoundedPrim k -> BoundedPrim v
             -> [(k, v)] -> Builder

  genMsgEnd :: Proxy p -> Builder
  genMsgEnd _ = mempty

  -- Primitive Generators (for base types)
  genBytePrim   :: Proxy p -> BoundedPrim Int8
  genI16Prim    :: Proxy p -> BoundedPrim Int16
  genI32Prim    :: Proxy p -> BoundedPrim Int32
  genI64Prim    :: Proxy p -> BoundedPrim Int64
  genBoolPrim   :: Proxy p -> BoundedPrim Bool


  -- Generators for string types (implement 1)
  genText   :: Proxy p -> Text -> Builder
  genText p = genByteString p . encodeUtf8
  genByteString :: Proxy p -> ByteString -> Builder
  genByteString p = genText p . decodeUtf8

  -- Generator for binary
  genBytes :: Proxy p -> ByteString -> Builder
  genBytes = genByteString

  -- Generators for other base types
  genFloat  :: Proxy p -> Float  -> Builder
  genDouble :: Proxy p -> Double -> Builder

  -- Parsers for tokens
  parseMsgBegin   :: Proxy p -> Parser MsgBegin
  parseMsgEnd     :: Proxy p -> Parser ()
  parseMsgEnd _ = return ()

  parseFieldBegin :: Proxy p -> FieldId -> IdMap -> Parser FieldBegin
  parseList :: Proxy p -> Parser a -> Parser (Int,[a])
  parseMap  :: Proxy p -> Parser k -> Parser v -> Bool -> Parser [(k, v)]

  -- Parser for base types
  parseByte   :: Proxy p -> Parser Int8
  parseI16    :: Proxy p -> Parser Int16
  parseI32    :: Proxy p -> Parser Int32
  parseI64    :: Proxy p -> Parser Int64
  parseFloat  :: Proxy p -> Parser Float
  parseDouble :: Proxy p -> Parser Double
  parseBool   :: Proxy p -> Parser Bool
  parseBoolF  :: Proxy p -> Bool -> Parser Bool

  -- Parsers for strings
  parseText :: Proxy p -> Parser Text
  parseText p = decodeUtf8 <$> parseByteString p
  parseByteString :: Proxy p -> Parser ByteString
  parseByteString p = encodeUtf8 <$> parseText p

  -- Parser for binary
  parseBytes :: Proxy p -> Parser ByteString
  parseBytes = parseByteString

  parseSkip :: Proxy p -> Type -> Maybe Bool -> Parser ()

genByte   :: Protocol p => Proxy p -> Int8   -> Builder
genByte p = primBounded (genBytePrim p)

genI16    :: Protocol p => Proxy p -> Int16  -> Builder
genI16 p = primBounded (genI16Prim p)

genI32    :: Protocol p => Proxy p -> Int32  -> Builder
genI32 p = primBounded (genI32Prim p)

genI64    :: Protocol p => Proxy p -> Int64  -> Builder
genI64 p = primBounded (genI64Prim p)

genBool   :: Protocol p => Proxy p -> Bool   -> Builder
genBool p = primBounded (genBoolPrim p)

parseStop :: Protocol p => Proxy p -> Parser ()
parseStop token = do
  end <- parseFieldBegin token 0 HashMap.empty
  case end of
    FieldBegin{} -> fail "parseStop: expected end of struct"
    FieldEnd -> return ()

parseEnum
  :: (Protocol p, ThriftEnum a)
  => Proxy p
  -> String
  -> Parser a
parseEnum proxy _ = do
  n <- parseI32 proxy
  return $ toThriftEnum (fromIntegral n)

parseEnumNoUnknown
  :: (Protocol p, ThriftEnum a)
  => Proxy p
  -> String
  -> Parser a
parseEnumNoUnknown proxy name = do
  n <- parseI32 proxy
  case toThriftEnumEither (fromIntegral n) of
    Left _ -> fail $
      "parseEnum: not a valid identifier for thrift enum '" ++ name ++ "': "
        ++ show n
    Right enum -> return enum

unknownThriftEnumErrorMsg :: (Typeable a, Show a) => a -> String
unknownThriftEnumErrorMsg x = "Unknown value "
  ++ show x
  ++ " for enum "
  ++ show (tyConModule $ typeRepTyCon $ typeOf x)
  ++ "."
  ++ show (typeOf x)
