-- Copyright (c) Facebook, Inc. and its affiliates.

module IOBufBench where

import Control.DeepSeq
import Criterion.Main
import Data.ByteString.Builder
import Data.ByteString.Unsafe
import Data.Default
import Data.Proxy
import Foreign.C
import Foreign.Ptr
import Util.IOBuf
import qualified Data.ByteString.Lazy as Lazy

import Thrift.Protocol
import Thrift.Protocol.Compact

import HsTest.Types

simpleObj :: TestStruct
simpleObj = def

simpleCereal :: Lazy.ByteString
simpleCereal = force $ toLazyByteString $ buildStruct compactProxy simpleObj

complexObj :: TestStruct
complexObj = def
  { testStruct_f_i64 = 0xCCCCCCCC
  , testStruct_f_list = [0..1023]
  }

complexCereal :: Lazy.ByteString
complexCereal = force $ toLazyByteString $ buildStruct compactProxy complexObj

compactProxy :: Proxy Compact
compactProxy = Proxy

marshalOld :: Lazy.ByteString -> IO ()
marshalOld bs =
  unsafeUseAsCStringLen (Lazy.toStrict bs) $ \(buf, len) ->
  c_marshal_bytestring buf (fromIntegral len)

marshalNew :: Lazy.ByteString -> IO ()
marshalNew bs = unsafeWithIOBuf bs $ c_marshal_iobuf

main :: IO ()
main = do
  defaultMain [ bgroup "simple" $ mkBench simpleCereal
              , bgroup "complex" $ mkBench complexCereal
              ]
  print $ length $ Lazy.toChunks simpleCereal
  print $ length $ Lazy.toChunks complexCereal
  where
    mkBench cereal =
      [ bench "marshal ByteString" $ nfIO $ marshalOld cereal
      , bench "marshal IOBuf" $ nfIO $ marshalNew cereal
      ]

--------------------------------------------------------------------------------

foreign import ccall unsafe "marshal_bytestring"
  c_marshal_bytestring :: CString -> CSize -> IO ()

foreign import ccall unsafe "marshal_iobuf"
  c_marshal_iobuf :: Ptr IOBuf -> IO ()
