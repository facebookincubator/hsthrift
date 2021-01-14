-- Copyright (c) Facebook, Inc. and its affiliates.

module MessageTest where

import Control.Exception
import Thrift.Binary.Parser hiding (peek)
import Data.ByteString (packCStringLen, useAsCStringLen)
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict)
import Data.Proxy
import Data.Text (Text)
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Exit
import Test.QuickCheck as QC
import qualified Data.Text as Text

import Thrift.Protocol
import Thrift.Protocol.Binary
import Thrift.Protocol.Compact
import Thrift.Protocol.JSON

import HsTest.Types

newtype Name = N Text
    deriving Show
instance Arbitrary Name where
  arbitrary = N. Text.pack . filter (/= '\NUL') <$> arbitrary

newtype MessageType = MT MsgType
    deriving Show
instance Arbitrary MessageType where
  arbitrary = MT <$> elements [1..4]

prop_roundtrip
  :: Protocol p
  => Proxy p
  -> (CString -> CSize -> Ptr CString -> IO CSize)
  -> Name
  -> MessageType
  -> SeqNum
  -> Foo
  -> Property
prop_roundtrip proxy echo (N name) (MT msgType) seqNum foo = ioProperty $
  useAsCStringLen (serialize name msgType seqNum) $ \(str, len) ->
  alloca $ \ptr ->
    bracket
      (do size <- fromIntegral <$> echo str (fromIntegral len) ptr
          buf  <- peek ptr
          return (buf, size))
      (free . fst)
      (\cstr ->
       if fst cstr == nullPtr
       then return False
       else do
         cereal <- packCStringLen cstr
         return $ deserialize cereal == Right (msgBegin, foo))
  where
    serialize n m s =
      toStrict $ toLazyByteString $
      genMsgBegin proxy n m s <>
      buildStruct proxy foo   <>
      genMsgEnd proxy
    deserialize = parse $ do
      mBegin <- parseMsgBegin proxy
      struct <- parseStruct proxy
      parseMsgEnd proxy
      return (mBegin, struct)
    msgBegin = MsgBegin name msgType seqNum

main :: IO ()
main = do
  result <- sequence
    [ quickCheckResult $ prop_roundtrip (Proxy :: Proxy Binary) c_echoBinary
    , quickCheckResult $ prop_roundtrip (Proxy :: Proxy Compact) c_echoCompact
    , quickCheckResult $ prop_roundtrip (Proxy :: Proxy JSON) c_echoJSON
    ]
  if all success result then exitSuccess else exitFailure
    where
      success QC.Success{} = True
      success _ = False

--------------------------------------------------------------------------------

foreign import ccall unsafe "echoBinary"
  c_echoBinary :: CString -> CSize -> Ptr CString -> IO CSize

foreign import ccall unsafe "echoCompact"
  c_echoCompact :: CString -> CSize -> Ptr CString -> IO CSize

foreign import ccall unsafe "echoJSON"
  c_echoJSON :: CString -> CSize -> Ptr CString -> IO CSize
