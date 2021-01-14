-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
module Thrift.Protocol.Compact
  ( serializeCompact
  , deserializeCompact
  , Compact
  ) where

import Control.Monad
import Thrift.Binary.Parser as P
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Builder as B
import Data.ByteString.Builder.Prim as Prim
import Data.Int
import Data.List
import Data.Proxy
import Data.Text.Encoding
import Data.Word
import GHC.Base hiding (build, Type)
import GHC.Word
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap

import Thrift.Protocol
import Thrift.Protocol.Binary.Internal

data Compact

serializeCompact :: ThriftSerializable a => a -> ByteString
serializeCompact = serializeGen (Proxy :: Proxy Compact)

deserializeCompact :: ThriftSerializable a => ByteString -> Either String a
deserializeCompact = deserializeGen (Proxy :: Proxy Compact)

-- Macros for ThriftTypes ------------------------------------------------------

#define TSTOP   0x00
#define TTRUE   0x01
#define TFALSE  0x02
#define TBYTE   0x03
#define TI16    0x04
#define TI32    0x05
#define TI64    0x06
#define TDOUBLE 0x07
#define TSTRING 0x08
#define TLIST   0x09
#define TSET    0x0A
#define TMAP    0x0B
#define TSTRUCT 0x0C
#define TFLOAT  0x0D

protocolVersion :: Word8
protocolVersion = 0x02

protocolID :: Word8
protocolID = 0x82

typeMask :: Word8
typeMask = 0xE0

versionMask :: Word8
versionMask = 0x1F

shiftAmt :: Int
shiftAmt = 5

-- BinaryWriter instance -------------------------------------------------------

instance Protocol Compact where
  -- Generators for Types
  getByteType   _ = TBYTE
  getI16Type    _ = TI16
  getI32Type    _ = TI32
  getI64Type    _ = TI64
  getFloatType  _ = TFLOAT
  getDoubleType _ = TDOUBLE
  getBoolType   _ = TTRUE
  getStringType _ = TSTRING
  getListType   _ = TLIST
  getSetType    _ = TSET
  getMapType    _ = TMAP
  getStructType _ = TSTRUCT

  -- Generators for tokens
  genMsgBegin proxy name msgType seqNum =
    primBounded
      (liftFixedToBounded Prim.word8 >*<
       liftFixedToBounded Prim.word8 >*<
       compactI32)
      (protocolID,
       (protocolVersion .|. ((msgType `shiftL` shiftAmt) .&. typeMask),
        seqNum)) <>
    genText proxy name

  genStruct _ fields = mconcat fields <> B.word8 TSTOP

  -- NOTE: This will not work correctly for generating boolean fields. Boolean
  -- fields MUST be generated using genFieldPrim
  genField _ _ ty fid lastId val =
    primBounded (compactFieldBegin ty) (fid, lastId) <> val

  genFieldPrim _ _ ty fid lastId prim val =
    primBounded (compactFieldBegin ty >*< prim) ((fid, lastId), val)

  genFieldBool _ _ fid lastId val =
    primBounded (compactFieldBegin ty) (fid, lastId)
    where
      ty = if val then TTRUE else TFALSE

  genList _ ty build elems =
    genListBegin ty elems <>
    mconcat (map build elems)

  genListPrim _ ty bounded elems =
    genListBegin ty elems <>
    primMapListBounded bounded elems

  genMap _ kt vt _ kbuild vbuild elems =
    genMapBegin kt vt elems <>
    mconcat (map (\(k, v) -> kbuild k <> vbuild v) elems)

  genMapPrim _ kt vt _ kbuild vbuild elems =
    genMapBegin kt vt elems <>
    primMapListBounded (kbuild >*< vbuild) elems

  -- Generators for base types
  genBytePrim _ = liftFixedToBounded Prim.int8
  genI16Prim  _ = compactI16
  genI32Prim  _ = compactI32
  genI64Prim  _ = compactI64
  genFloat  _ = B.floatBE
  genDouble _ = B.doubleBE
  genBoolPrim _ = liftFixedToBounded $ getVal >$< Prim.word8
    where getVal b = if b then TTRUE else TFALSE

  genByteString _ s =
    primBounded buildVarint (W# (int2Word# len)) <> byteString s
    where
      !(I# len) = BS.length s

  -- Parsers for tokens
  parseMsgBegin proxy = do
    pid <- P.anyWord8
    when (pid /= protocolID) $ fail "parseMsgBegin: invalid protocol id"
    versionAndType <- P.anyWord8
    let
      version = versionAndType .&. versionMask
      msgType = (versionAndType .&. typeMask) `shiftR` shiftAmt
    when (version /= protocolVersion) $
      fail "parseMsgBegin: invalid protocol version"
    seqNum <- parseCompactI32
    name   <- parseText proxy
    return $ MsgBegin name msgType seqNum

  parseFieldBegin _ lastId _ = do
    byte <- anyWord8
    let rawType = byte .&. 0x0F
    if rawType == TSTOP
    then pure FieldEnd
    else do
      -- Boolean Value is encoded in the type
      let (!ty, !bool) =
            case rawType of
              0x01 -> (TTRUE, True)
              0x02 -> (TTRUE, False)
              mask -> (mask, False)
      -- Field Id depends on previous field Id
      fid <- case byte `shiftR` 4 of
               0x00 -> parseCompactI16
               offs -> pure $ fromIntegral offs + lastId
      return $ FieldBegin ty fid bool

  parseList _ p = do
    byte <- anyWord8
    let _ty = byte .&. 0x0F
    len <- case byte `shiftR` 4 of
             0x0F -> parseVarint32
             size -> pure $ fromIntegral size
    ps <- replicateM (fromIntegral len) p
    return (fromIntegral len, ps)

  parseMap _ pk pv _ = do
    len <- parseVarint32
    if len == 0
    then pure []
    else do
      byte <- anyWord8
      let _ktype = byte `shiftR` 4
          _vtype = byte .&. 0x0F
      replicateM (fromIntegral len) $ (,) <$> pk <*> pv

  -- Parser for base types
  parseByte   _ = fromIntegral <$> anyWord8
  parseI16    _ = parseCompactI16
  parseI32    _ = parseCompactI32
  parseI64    _ = parseCompactI64
  parseFloat  _ = binaryFloat
  parseDouble _ = binaryDouble
  parseBool   _ = do
    byte <- P.anyWord8
    case byte of
      TTRUE -> pure True
      TFALSE -> pure False
      n -> fail $ "invalid boolean value: " ++ show n

  parseBoolF  _ = pure
  parseByteString _ = getBuffer parseVarint32 BS.copy
  parseText _ = getBuffer parseVarint32 decodeUtf8

  parseSkip _ TTRUE Nothing = P.skipN 1
  parseSkip _ TFALSE Nothing = P.skipN 1
  parseSkip _ TTRUE Just{}  = pure ()
  parseSkip _ TFALSE Just{}  = pure ()
  parseSkip _ TBYTE _ = P.skipN 1
  parseSkip _ TI16 _ = void parseCompactI16
  parseSkip _ TI32 _ = void parseCompactI32
  parseSkip _ TI64 _ = void parseCompactI64
  parseSkip _ TDOUBLE _ = P.skipN 8
  parseSkip _ TSTRING _ =
    void $ parseVarint32 >>= P.skipN . fromIntegral
  parseSkip proxy TLIST   _ = do
    byte <- anyWord8
    let ty = byte .&. 0x0F
    len <- case byte `shiftR` 4 of
             0x0F -> parseVarint32
             size -> pure $ fromIntegral size
    void $ replicateM (fromIntegral len) (parseSkip proxy ty Nothing)

  parseSkip proxy TSET _ = parseSkip proxy TLIST Nothing
  parseSkip proxy TMAP _ = do
    len <- parseVarint32
    if len == 0
    then pure ()
    else do
      byte <- anyWord8
      let ktype = byte `shiftR` 4
          vtype = byte .&. 0x0F
      void $ replicateM (fromIntegral len) $
        (,) <$> parseSkip proxy ktype Nothing <*> parseSkip proxy vtype Nothing

  parseSkip proxy TSTRUCT _ = do
    -- The last id deosn't matter since we do not need to correctly parse field
    -- ids. We don't recognize this field, therefore it will be discarded anyway
    fieldBegin <- parseFieldBegin proxy 0 HashMap.empty
    case fieldBegin of
      FieldBegin ty _ bool ->
        parseSkip proxy ty (Just bool) *> parseSkip proxy TSTRUCT Nothing
      FieldEnd -> pure ()
  parseSkip _ n _ = fail $ "unrecognized type code: " ++ show n

-- Helpers ---------------------------------------------------------------------

{-# INLINE compactFieldBegin #-}
compactFieldBegin :: Type -> BoundedPrim (FieldId, FieldId)
compactFieldBegin ty = condB
  (\(fid, lastId) -> fid > lastId && fid - lastId < 16)
  ((\(fid, lastId) -> fromIntegral (fid - lastId) `shiftL` 4 .|. ty) >$<
   liftFixedToBounded Prim.word8)
  ((\(fid, _) -> (ty, fid)) >$<
   liftFixedToBounded Prim.word8 >*< compactI16)

compactI16 :: BoundedPrim Int16
compactI16 = i16ToZigZag >$< buildVarint2

compactI32 :: BoundedPrim Int32
compactI32 = i32ToZigZag >$< buildVarint4

compactI64 :: BoundedPrim Int64
compactI64 = i64ToZigZag >$< buildVarint

parseCompactI16 :: Parser Int16
parseCompactI16 = zigZagToInt <$> parseVarint

parseCompactI32 :: Parser Int32
parseCompactI32 = zigZagToInt <$> parseVarint

parseCompactI64 :: Parser Int64
parseCompactI64 = zigZagToInt <$> parseVarint

genListBegin :: Type -> [a] -> Builder
genListBegin ty elems = (`primBounded` len) $
  condB (< 15)
    ((\l -> (fromIntegral l `shiftL` 4) .|. ty) >$<
     liftFixedToBounded Prim.word8)
    ((\l -> (0xF0 .|. ty, fromIntegral l)) >$<
     (liftFixedToBounded Prim.word8 >*< buildVarint))
  where len = length elems

genMapBegin :: Type -> Type -> [a] -> Builder
genMapBegin kt vt elems
  | null elems = B.word8 0x00
  | otherwise = (`primBounded` len) $
      (\l -> (l, kt `shiftL` 4 .|. vt)) >$<
      (buildVarint >*< liftFixedToBounded Prim.word8)
    where len = genericLength elems

-- Variable Length Encoded Integers --------------------------------------------

-- Signed numbers must be converted to "Zig Zag" format before they can be
-- serialized in the Varint format

{-# INLINE iToZigZag #-}
iToZigZag :: (Bits i, Integral i) => Int -> i -> Word
iToZigZag s n = fromIntegral $ (n `shiftL` 1) `xor` (n `shiftR` s)

{-# INLINE i16ToZigZag #-}
i16ToZigZag :: Int16 -> Word
i16ToZigZag n = 0xFFFF .&. iToZigZag 15 n

{-# INLINE i32ToZigZag #-}
i32ToZigZag :: Int32 -> Word
i32ToZigZag n = 0xFFFFFFFF .&. iToZigZag 31 n

{-# INLINE i64ToZigZag #-}
i64ToZigZag :: Int64 -> Word
i64ToZigZag = iToZigZag 63

-- This is janky, but BoundedPrim requires the size of the value it is building
-- to be bounded. Varints *are* bounded, but the output size depends on their
-- value. A 64-bit value can take up to 80 bits to encode, because each 7-bits
-- in the input map to 8 bits in the output. We therefore need 10 builders, each
-- one decreasing in potential maximum size.
buildVarint :: BoundedPrim Word
buildVarint =  buildVarintBase buildVarint8

buildVarint8 :: BoundedPrim Word
buildVarint8 = buildVarintBase buildVarint7

buildVarint7 :: BoundedPrim Word
buildVarint7 = buildVarintBase buildVarint6

buildVarint6 :: BoundedPrim Word
buildVarint6 = buildVarintBase buildVarint5

buildVarint5 :: BoundedPrim Word
buildVarint5 = buildVarintBase buildVarint4

buildVarint4 :: BoundedPrim Word
buildVarint4 = buildVarintBase buildVarint3

buildVarint3 :: BoundedPrim Word
buildVarint3 = buildVarintBase buildVarint2

buildVarint2 :: BoundedPrim Word
buildVarint2 = buildVarintBase buildVarint1

buildVarint1 :: BoundedPrim Word
buildVarint1 = buildVarintBase buildVarint0

buildVarint0 :: BoundedPrim Word
buildVarint0 = fromIntegral >$< liftFixedToBounded Prim.word8

{-# INLINE buildVarintBase #-}
buildVarintBase :: BoundedPrim Word -> BoundedPrim Word
buildVarintBase base = condB (\n -> n .&. complement 0x7F == 0)
  buildVarint0
  ((\(W# n) ->
     (W8# (0x80## `or#` (n `and#` 0x7f##)), W# (n `uncheckedShiftRL#` 7#))) >$<
   (liftFixedToBounded Prim.word8 >*< base))

{-# INLINE zigZagToInt #-}
zigZagToInt :: (Bits i, Integral i) => Word -> i
zigZagToInt w = fromIntegral (w `shiftR` 1) `xor` negate (fromIntegral w .&. 1)

{-# INLINE parseVarint #-}
parseVarint :: Parser Word
parseVarint = go 0 0
  where
    go !val !n = do
      w <- anyWord8
      let newVal = val .|. (fromIntegral w .&. 0x7F) `shiftL` n
      if not (testBit w 7)
        then return newVal
        else go newVal (n + 7)

parseVarint32 :: Parser Int32
parseVarint32 = fromIntegral <$> parseVarint
