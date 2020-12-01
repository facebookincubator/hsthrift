-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Strict #-}

module Util.Binary.Parser
  ( Parser
  , parse
  , parseAndLeftover
  , peekBS
  , anyWord8
  , word8
  , getInt16be
  , getInt16le
  , getInt32be
  , getInt32le
  , getInt64be
  , getInt64le
  , getWord16be
  , getWord16le
  , getWord32be
  , getWord32le
  , getWord64be
  , getWord64le
  , skipN
  , skipSpaces
  , sepBy
  , string
  , signed
  , peek
  , double
  , decimal
  , getByteString
  , takeWhile
  ) where

import Control.Applicative
#if __GLASGOW_HASKELL__ == 806
import Control.Monad hiding (fail)
#else
import Control.Monad
#endif
#if __GLASGOW_HASKELL__ == 806
import Control.Monad.Fail as Fail
#endif
import Control.Exception
import Util.Control.Exception (tryAll)

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe as B
import qualified Data.ByteString as B
import qualified Data.ByteString.Lex.Integral as LexInt
import Data.Scientific (Scientific (..))
import qualified Data.Scientific as Sci
import Data.Word

import FastMutInt

#if __GLASGOW_HASKELL__ == 806
import GHC.Base hiding (fail)
#else
import GHC.Base
#endif
import GHC.Word
import GHC.Int

#if __GLASGOW_HASKELL__ == 806
import Prelude hiding (takeWhile, fail)
#else
import Prelude hiding (takeWhile)
#endif

import System.IO.Unsafe (unsafeDupablePerformIO)

data Env = Env {-# UNPACK #-} !ByteString
               {-# UNPACK #-} !FastMutInt

newtype Parser a = Parser { unParser :: Env -> IO a }

instance Functor Parser where
  fmap f m = Parser $ \env -> do
    a <- unParser m env
    return $ f a
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure  = return
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Alternative Parser where
  empty = Parser $ \_ -> throwIO $
    ParseError "Thrift.Binary.Parser(Alternative).empty"
  (<|>) f g = Parser $ \env@(Env _ mutPos) -> do
    originalPos <- readFastMutInt mutPos
    eitherA <- tryAll $ unParser f env
    case eitherA of
      Right a -> return a
      Left _ -> do
        writeFastMutInt mutPos originalPos
        unParser g env

newtype ParseError = ParseError String deriving Show

instance Exception ParseError

instance Monad Parser where
    return a = Parser $ \_ -> return a
    {-# INLINE return #-}

    m >>= k = Parser $ \env -> do
      a <- unParser m env
      unParser (k a) env
    {-# INLINE (>>=) #-}

#if __GLASGOW_HASKELL__ > 804
instance MonadFail Parser where
#endif
    fail err = Parser $ \ (Env _ mutPos) -> do
      pos <- readFastMutInt mutPos
      throwIO $ ParseError
        $ err ++ ". Failed reading at byte position " ++ show pos

get :: Parser ByteString
get = Parser $ \(Env bs mutPos) -> do
  pos <- readFastMutInt mutPos
  return $ B.unsafeDrop pos bs

getAndIncrementMutPos :: FastMutInt -> Int -> IO Int
getAndIncrementMutPos mutPos nBytes = do
  pos <- readFastMutInt mutPos
  writeFastMutInt mutPos (pos + nBytes)
  return pos
{-# INLINE getAndIncrementMutPos #-}

incrPos :: Int -> Parser ()
incrPos nBytes= Parser $ \(Env _ mutPos) -> void $
  getAndIncrementMutPos mutPos nBytes

parse :: Parser a -> ByteString -> Either String a
parse getter bs =
  let
    eitherA = unsafeDupablePerformIO $ do
      mutPos <- newFastMutInt
      writeFastMutInt mutPos 0
      tryAll $ unParser getter $ Env bs mutPos in
  case eitherA of
    Right a -> Right a
    Left err -> Left (show err)

parseAndLeftover
  :: Parser a -> ByteString -> Either String (a, ByteString)
parseAndLeftover p bs = parse p' bs
  where p' = (,) <$> p <*> peekBS

peekBS :: Parser ByteString
peekBS = get

ensureN :: Int -> Parser Int
ensureN nBytes
  | nBytes < 0 = fail $ "negative byte count: " ++ show nBytes
  | otherwise = Parser $ \(Env bs mutPos) -> do
      pos <- readFastMutInt mutPos
      unless (nBytes <= B.length bs - pos) $
        fail "incomplete input"
      return pos
{-# INLINE ensureN #-}

anyWord8 :: Parser Word8
anyWord8 = do
  pos <- ensureN 1
  Parser $ \(Env bs mutPos) -> do
    writeFastMutInt mutPos (pos + 1)
    return $ B.unsafeIndex bs pos
{-# INLINE anyWord8 #-}

getByteString :: Int -> Parser ByteString
getByteString nBytes = do
  pos <- ensureN nBytes
  Parser $ \(Env bs mutPos) -> do
    writeFastMutInt mutPos (pos + nBytes)
    return $ B.unsafeTake nBytes (B.unsafeDrop pos bs)
{-# INLINE getByteString #-}

readN :: Int -> (ByteString -> a) -> Parser a
readN n f = f <$> getByteString n
{-# INLINE [0] readN #-}

skipN :: Int -> Parser ()
skipN nBytes = do
  pos <- ensureN nBytes
  Parser $ \(Env _ mutPos) -> writeFastMutInt mutPos (pos + nBytes)

peek :: Parser Word8
peek = do
  pos <- ensureN 1
  Parser $ \(Env bs _) ->
    return $ B.unsafeIndex bs pos
{-# INLINE peek #-}

word8 :: Word8 -> Parser ()
word8 w8 = do
  myW8 <- anyWord8
  when (myW8 /= w8) $
    fail "word8"
{-# INLINE word8 #-}

takeWhile :: (Word8 -> Bool) -> Parser ByteString
takeWhile p = do
  bs <- get
  let (want, _) = B.span p bs
  incrPos $ B.length want
  return want
{-# INLINE takeWhile #-}

isSpace :: Word8 -> Bool
isSpace w = w == 32 || w - 9 <= 4
{-# INLINE isSpace #-}

skipSpaces :: Parser ()
skipSpaces = do
  bs <- get
  let rest = B.dropWhile isSpace bs
  incrPos $ B.length bs - B.length rest

string :: ByteString -> Parser ()
string bs = do
  let l = B.length bs
  myBs <- getByteString l
  when (myBs /= bs) $
    fail "string"
{-# INLINE string #-}


-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit w = w >= 48 && w <= 57
--
satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy p = do
  w8 <- anyWord8
  unless (p w8) $
    fail "satisfy"
  return w8
{-# INLINE satisfy #-}

-- | Similar to 'takeWhile', but requires the predicate to succeed on at least
-- one byte of input: it will fail if the predicate never returns 'True' or
-- reach the end of input
--
takeWhile1 :: (Word8 -> Bool) -> Parser ByteString
takeWhile1 p = do
    bs <- takeWhile p
    if B.null bs then fail "takeWhile1" else return bs
{-# INLINE takeWhile1 #-}


#define  MINUS    45
#define  PLUS     43
#define  LITTLE_E 101
#define  BIG_E    69
#define  DOT      46

-- | Parse a number with an optional leading @\'+\'@ or @\'-\'@ sign
-- character.
--
signed :: Num a => Parser a -> Parser a
signed p = do
  w <- peek
  if w == MINUS
    then skipN 1 >> negate <$> p
    else if w == PLUS then skipN 1 >> p else p
{-# SPECIALISE signed :: Parser Int -> Parser Int #-}
{-# SPECIALISE signed :: Parser Int8 -> Parser Int8 #-}
{-# SPECIALISE signed :: Parser Int16 -> Parser Int16 #-}
{-# SPECIALISE signed :: Parser Int32 -> Parser Int32 #-}
{-# SPECIALISE signed :: Parser Int64 -> Parser Int64 #-}
{-# SPECIALISE signed :: Parser Integer -> Parser Integer #-}

-- | Decimal digit predicate.
--
isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9
{-# INLINE isDigit #-}

-- | Parse and decode an unsigned decimal number.
--
decimal :: Integral a => Parser a
decimal = do
    bs <- takeWhile1 isDigit
    return $! LexInt.readDecimal_ bs
{-# SPECIALISE decimal :: Parser Int #-}
{-# SPECIALISE decimal :: Parser Int8 #-}
{-# SPECIALISE decimal :: Parser Int16 #-}
{-# SPECIALISE decimal :: Parser Int32 #-}
{-# SPECIALISE decimal :: Parser Int64 #-}
{-# SPECIALISE decimal :: Parser Integer #-}
{-# SPECIALISE decimal :: Parser Word #-}
{-# SPECIALISE decimal :: Parser Word8 #-}
{-# SPECIALISE decimal :: Parser Word16 #-}
{-# SPECIALISE decimal :: Parser Word32 #-}
{-# SPECIALISE decimal :: Parser Word64 #-}

double :: Parser Double
double = scientifically Sci.toRealFloat

-- | Parse a scientific number and convert to result using a user supply
-- function.
--
-- The syntax accepted by this parser is the same as for 'double'.
--
scientifically :: (Scientific -> a) -> Parser a
scientifically h = do
    sign <- peek
    when (sign == PLUS || sign == MINUS) (skipN 1)
    intPart <- decimal
    sci <- (do fracDigits <- word8 DOT >> takeWhile1 isDigit
               let e' = B.length fracDigits
                   intPart' = intPart * (10 ^ e')
                   fracPart = LexInt.readDecimal_ fracDigits
               parseE (intPart' + fracPart) e'
           ) <|> parseE intPart 0

    if sign /= MINUS then return $! h sci else return $! h (negate sci)
  where
    parseE c e =
        (do _ <- satisfy (\w -> w ==  LITTLE_E || w == BIG_E)
            Sci.scientific c . subtract e <$> signed decimal)
        <|> return (Sci.scientific c (negate e))
    {-# INLINE parseE #-}
{-# INLINE scientifically #-}

sepBy :: Alternative f => f a -> f s -> f [a]
sepBy p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure []) <|> pure []
{-# SPECIALIZE sepBy :: Parser a -> Parser s -> Parser [a] #-}

sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 p s = go
    where go = liftA2 (:) p ((s *> go) <|> pure [])
{-# SPECIALIZE sepBy1 :: Parser a -> Parser s -> Parser [a] #-}

------------------------------------------------------------------------
-- Following code is copied from Data.Binary.Get
------------------------------------------------------------------------
type Get a = Parser a

-- force GHC to inline getWordXX
{-# RULES
"getWord16be/readN" getWord16be = readN 2 word16be
"getWord16le/readN" getWord16le = readN 2 word16le
"getWord32be/readN" getWord32be = readN 4 word32be
"getWord32le/readN" getWord32le = readN 4 word32le
"getWord64be/readN" getWord64be = readN 8 word64be
"getWord64le/readN" getWord64le = readN 8 word64le #-}

-- | Read a Word16 in big endian format
getWord16be :: Get Word16
getWord16be = readN 2 word16be

word16be :: B.ByteString -> Word16
word16be = \s ->
        (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w16` 8) .|.
        (fromIntegral (s `B.unsafeIndex` 1))
{-# INLINE[2] getWord16be #-}
{-# INLINE word16be #-}

-- | Read a Word16 in little endian format
getWord16le :: Get Word16
getWord16le = readN 2 word16le

word16le :: B.ByteString -> Word16
word16le = \s ->
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE[2] getWord16le #-}
{-# INLINE word16le #-}

-- | Read a Word32 in big endian format
getWord32be :: Get Word32
getWord32be = readN 4 word32be

word32be :: B.ByteString -> Word32
word32be = \s ->
              (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 3) )
{-# INLINE[2] getWord32be #-}
{-# INLINE word32be #-}

-- | Read a Word32 in little endian format
getWord32le :: Get Word32
getWord32le = readN 4 word32le

word32le :: B.ByteString -> Word32
word32le = \s ->
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE[2] getWord32le #-}
{-# INLINE word32le #-}

-- | Read a Word64 in big endian format
getWord64be :: Get Word64
getWord64be = readN 8 word64be

word64be :: B.ByteString -> Word64
word64be = \s ->
              (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 7) )
{-# INLINE[2] getWord64be #-}
{-# INLINE word64be #-}

-- | Read a Word64 in little endian format
getWord64le :: Get Word64
getWord64le = readN 8 word64le

word64le :: B.ByteString -> Word64
word64le = \s ->
              (fromIntegral (s `B.unsafeIndex` 7) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )
{-# INLINE[2] getWord64le #-}
{-# INLINE word64le #-}

-- | Read an Int16 in big endian format.
getInt16be :: Get Int16
getInt16be = fromIntegral <$> getWord16be
{-# INLINE getInt16be #-}

-- | Read an Int32 in big endian format.
getInt32be :: Get Int32
getInt32be =  fromIntegral <$> getWord32be
{-# INLINE getInt32be #-}

-- | Read an Int64 in big endian format.
getInt64be :: Get Int64
getInt64be = fromIntegral <$> getWord64be
{-# INLINE getInt64be #-}

-- | Read an Int16 in little endian format.
getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16le
{-# INLINE getInt16le #-}

-- | Read an Int32 in little endian format.
getInt32le :: Get Int32
getInt32le =  fromIntegral <$> getWord32le
{-# INLINE getInt32le #-}

-- | Read an Int64 in little endian format.
getInt64le :: Get Int64
getInt64le = fromIntegral <$> getWord64le
{-# INLINE getInt64le #-}

------------------------------------------------------------------------
-- Unchecked shifts
shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#` i)

shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#` i)

shiftl_w64 :: Word64 -> Int -> Word64
#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)
#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif
