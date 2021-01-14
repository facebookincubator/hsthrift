-- Copyright (c) Facebook, Inc. and its affiliates.

{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
module Thrift.Protocol.JSON.String
  ( parseJSONString
  ) where

import Control.Exception
import Control.Monad
import Thrift.Binary.Parser
import Data.Bits
import qualified Data.ByteString as ByteString
import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Unsafe
import Data.Char
import Data.Text (Text)
import Data.Text.Encoding
import Data.Word
import Data.Word8
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

parseJSONString :: Parser Text
parseJSONString = do
  skipSpaces *> word8 _quotedbl
  rawBytes <- peekBS
  case unescape rawBytes of
    Left (StringError err) -> fail err
    Right (UnescapeResult bytes inLen) -> do
      skipN (inLen + 1) -- Skip over the string and closing "
      case decodeUtf8' bytes of
        Left err -> fail $ show err
        Right txt -> return txt

-- NOTE: The cpp2 implementation only accepts escaped "unicode" characters of
-- the form "\u00XX" where the X's are hex digits, so we will do the same here.
-- This means that all \u characters fit into a single byte which simplifies the
-- unescaping logic signifigantly

-- Datatype where all fields are strict so that we can avoid calling `evaluate`
data UnescapeResult = UnescapeResult !ByteString {-# UNPACK #-} !Int

-- Low-level unescaping logic. Works on the underlying byte array
unescape :: ByteString -> Either StringError UnescapeResult
unescape rawBytes = unsafeDupablePerformIO $ try $
  unsafeUseAsCStringLen rawBytes $ \(charBuf, fullLen) -> do
  let input = castPtr charBuf :: Ptr Word8

  -- Find the lengths of the input and output
  (inLen, outLen) <- unescapedLength input fullLen

  -- If the input has the same length as the output, then there are no escape
  -- characters, so we can just return the ByteString as is
  if inLen == outLen
    then return $! UnescapeResult (ByteString.take outLen rawBytes) inLen
    else do

  -- Allocate a buffer for the output
  outBuf <- mallocForeignPtrArray outLen
  withForeignPtr outBuf $ \output -> do
    let
      -- Unescape the input character by character.
      -- i is the input index and j is the output index
      go !i !j
        | j >= outLen = return ()
        | otherwise = do
            byte <- peekByteOff input i
            if
              -- Case 1: we have reached an escape character
              -- NOTE: no bounds checks are needed because we already checked
              -- bounds in unescapeLength
              | byte == _backslash -> do
                  escChr <- peekByteOff input $ i + 1
                  if
                    -- Case 1.1: We have reached a 4-hex-digit unicode point
                    | escChr == _u -> do
                        c <- parseUnicodeChar =<<
                             peekByteOff (castPtr input) (i + 2)
                        pokeByteOff output j c
                        go (i + 6) (j + 1)
                    -- Case 1.2: Byte is a valid escape char
                    | Just unesc <- unescapeChar escChr -> do
                        pokeByteOff output j unesc
                        go (i + 2) (j + 1)
                    -- Case 1.3: Invalid escape character
                    | otherwise ->
                        throwIO $ StringError $
                        "parseJSONString: Not a valid escape character: " ++
                        [chr $ fromIntegral byte]
              -- Case 2: Not an escape character
              | otherwise -> do
                  pokeByteOff output j byte
                  go (i + 1) (j + 1)
    go 0 0
  return $! UnescapeResult (PS outBuf 0 outLen) inLen

{-# INLINE unescapedLength #-}
-- Get the escaped and unescaped lengths of the input
unescapedLength :: Ptr Word8 -> Int -> IO (Int, Int)
unescapedLength buf len = go 0 0
  where
    -- In this function i is the index in the input and j is the output length
    go !i !j
      | i >= len = stringError
      | otherwise = do
          byte <- peekByteOff buf i
          if | byte == _quotedbl  -> return (i, j)
             | byte == _backslash ->
                 if i + 1 < len
                 then do
                   nextByte <- peekByteOff buf (i + 1)
                   if | nextByte == _u -> go (i + 6) (j + 1) -- skip over \uXXXX
                      | otherwise      -> go (i + 2) (j + 1) -- skip over \X
                 else stringError
             | otherwise -> go (i + 1) (j + 1)
    stringError =
      throwIO $ StringError "parseJSONString: string does not terminate"

{-# INLINE unescapeChar #-}
unescapeChar :: Word8 -> Maybe Word8
unescapeChar = \case
  0x22 {- " -} -> Just _quotedbl
  0x27 {- ' -} -> Just _quotesingle
  0x2F {- / -} -> Just _slash
  0x5C {- \ -} -> Just _backslash
  0x30 {- 0 -} -> Just _0
  0x61 {- a -} -> Just $ fromIntegral $ ord '\a'
  0x62 {- b -} -> Just $ fromIntegral $ ord '\b'
  0x66 {- f -} -> Just $ fromIntegral $ ord '\f'
  0x6E {- n -} -> Just _lf -- linefeed
  0x72 {- r -} -> Just _cr -- carriage return
  0x74 {- t -} -> Just _tab
  0x76 {- v -} -> Just _vt -- vertical tab
  _ -> Nothing

{-# INLINE parseUnicodeChar #-}
parseUnicodeChar :: Word32 -> IO Word8
parseUnicodeChar !word = do
  -- Must have the form '\u00XX' (see note above).
  -- 0x30 is the acsii character '0'
  unless (word .&. 0x0000FFFF == 0x3030) unicodeError
  d0 <- hexdigit $ fromIntegral $ (word `shiftR` 16) .&. 0x000000FF
  d1 <- hexdigit $ fromIntegral $ (word `shiftR` 24) .&. 0x000000FF
  return $ d0 `shiftL` 4 .|. d1
  where
    unicodeError = throwIO $ StringError "Improperly formatted unicode string"
    hexdigit :: Word8 -> IO Word8
    hexdigit n
      | n `inRange` (_0, _9) = pure $ n - _0
      | n `inRange` (_a, _f) = pure $ n - _a + 10
      | n `inRange` (_A, _F) = pure $ n - _A + 10
      | otherwise = unicodeError

inRange :: Word8 -> (Word8, Word8) -> Bool
inRange n (x, y) = n >= x && n <= y

newtype StringError = StringError String
  deriving Show

instance Exception StringError
