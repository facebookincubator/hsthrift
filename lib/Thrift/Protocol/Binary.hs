-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
module Thrift.Protocol.Binary
  ( serializeBinary, deserializeBinary
  , Binary
  ) where

import Control.Monad
import Thrift.Binary.Parser
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.ByteString.Builder.Prim as Prim
import Data.List
import Data.Proxy
import Data.Text.Encoding
import Data.Word
import qualified Thrift.Binary.Parser as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.HashMap.Strict as HashMap

import Thrift.Protocol
import Thrift.Protocol.Binary.Internal

data Binary

serializeBinary :: ThriftSerializable a => a -> ByteString
serializeBinary = serializeGen (Proxy :: Proxy Binary)

deserializeBinary :: ThriftSerializable a => ByteString -> Either String a
deserializeBinary = deserializeGen (Proxy :: Proxy Binary)

-- Type Macros -----------------------------------------------------------------

#define TSTOP   0
#define TBOOL   2
#define TBYTE   3
#define TDOUBLE 4
#define TI16    6
#define TI32    8
#define TI64    10
#define TSTRING 11
#define TSTRUCT 12
#define TMAP    13
#define TSET    14
#define TLIST   15
#define TFLOAT  19

version1 :: Word32
version1 = 0x80010000

versionMask :: Word32
versionMask = 0xFFFF0000

-- BinaryProtocol instance -----------------------------------------------------

instance Protocol Binary where
  -- Generators for Types
  getByteType   _ = TBYTE
  getI16Type    _ = TI16
  getI32Type    _ = TI32
  getI64Type    _ = TI64
  getFloatType  _ = TFLOAT
  getDoubleType _ = TDOUBLE
  getBoolType   _ = TBOOL
  getStringType _ = TSTRING
  getListType   _ = TLIST
  getSetType    _ = TSET
  getMapType    _ = TMAP
  getStructType _ = TSTRUCT

  -- Generators for tokens
  genMsgBegin proxy name msgType seqNum =
    B.word32BE (version1 .|. fromIntegral msgType) <>
    genText proxy name <>
    B.int32BE seqNum

  genStruct _ fields = mconcat fields <> B.word8 TSTOP

  genField _ _ ty fid _ val = B.word8 ty <> B.int16BE fid <> val
  genFieldPrim _ _ ty fid _ prim val = primBounded
    (liftFixedToBounded (Prim.word8 >*< Prim.int16BE) >*< prim)
    ((ty, fid), val)

  genList _ ty build elems =
    genListBegin ty elems <> mconcat (map build elems)

  genListPrim _ ty build elems =
    genListBegin ty elems <> primMapListBounded build elems

  genMap _ kt vt _ kbuild vbuild elems =
    genMapBegin kt vt elems <> mconcat (map build elems)
    where
      build (k, v) = kbuild k <> vbuild v

  genMapPrim _ kt vt _ kbuild vbuild elems =
    genMapBegin kt vt elems <> primMapListBounded build elems
    where
      build = kbuild >*< vbuild

  -- Generators for base types
  genBytePrim _ = liftFixedToBounded Prim.int8
  genI16Prim  _ = liftFixedToBounded Prim.int16BE
  genI32Prim  _ = liftFixedToBounded Prim.int32BE
  genI64Prim  _ = liftFixedToBounded Prim.int64BE
  genFloat _ = B.floatBE
  genDouble _ = B.doubleBE
  genBoolPrim _ = liftFixedToBounded $
    (\b -> if b then 1 else 0) >$< Prim.word8
  genByteString _ s = B.int32BE len <> byteString s
    where
      len = fromIntegral $ BS.length s

  -- Parsers for tokens
  parseMsgBegin proxy = do
    versionAndType <- getWord32be
    let
      version = versionAndType .&. versionMask
      msgType = fromIntegral (versionAndType .&. 0x000000FF)
    when (version /= version1) $ fail "parseMsgBegin: invalid version"
    name <- parseText proxy
    seqNum <- getInt32be
    return $ MsgBegin name msgType seqNum

  parseFieldBegin _ _ _ = do
    ty <- anyWord8
    if ty == TSTOP
    then pure FieldEnd
    else FieldBegin ty <$> getInt16be <*> pure False

  parseList _ p = do
    _ <- anyWord8
    len <- getInt32be
    ps <- replicateM (fromIntegral len) p
    return (fromIntegral len, ps)

  parseMap _ pk pv _ = do
    _ <- anyWord8
    _ <- anyWord8
    len <- getInt32be
    replicateM (fromIntegral len) $ (,) <$> pk <*> pv

  -- Parser for base types
  parseByte   _ = fromIntegral <$> P.anyWord8
  parseI16    _ = getInt16be
  parseI32    _ = getInt32be
  parseI64    _ = getInt64be
  parseFloat  _ = binaryFloat
  parseDouble _ = binaryDouble
  parseBool   _ = binaryBool
  parseBoolF _ _ = binaryBool
  parseByteString _ = getBuffer getInt32be BS.copy
  parseText _ = getBuffer getInt32be decodeUtf8

  parseSkip _ TBYTE   _ = skipN 1
  parseSkip _ TI16    _ = skipN 2
  parseSkip _ TI32    _ = skipN 4
  parseSkip _ TI64    _ = skipN 8
  parseSkip _ TFLOAT  _ = skipN 4
  parseSkip _ TDOUBLE _ = skipN 8
  parseSkip _ TBOOL   _ = skipN 1
  parseSkip _ TSTRING _ = getInt32be >>= skipN . fromIntegral
  parseSkip proxy TLIST _ = skipList proxy
  parseSkip proxy TSET  _ = skipList proxy
  parseSkip proxy TMAP  _ = do
    k <- anyWord8
    v <- anyWord8
    len <- getInt32be
    replicateM_ (fromIntegral len) $
      (,) <$> parseSkip proxy k Nothing <*> parseSkip proxy v Nothing
  parseSkip proxy TSTRUCT _ = do
    fbegin <- parseFieldBegin proxy 0 HashMap.empty
    case fbegin of
      FieldBegin ty _ _ -> parseSkip proxy ty Nothing *>
                           parseSkip proxy TSTRUCT Nothing
      FieldEnd -> pure ()
  parseSkip _ n _ = fail $ "unrecognized type code: " ++ show n

skipList :: Proxy Binary -> Parser ()
skipList proxy = do
  ty <- anyWord8
  len <- getInt32be
  replicateM_ (fromIntegral len) (parseSkip proxy ty Nothing)

genListBegin :: Type -> [a] -> Builder
genListBegin ty elems =
  primFixed (Prim.word8 >*< Prim.word32BE) (ty, len)
  where len = genericLength elems

genMapBegin :: Type -> Type -> [a] -> Builder
genMapBegin kt vt elems =
  primFixed
    (Prim.word8 >*< Prim.word8 >*< Prim.word32BE)
    (kt, (vt, len))
  where
    len = genericLength elems

{-# INLINE binaryBool #-}
binaryBool :: Parser Bool
binaryBool = do
  byte <- P.anyWord8
  case byte of
    0 -> pure False
    1 -> pure True
    n -> fail $ "invalid boolean value: " ++ show n
