-- Copyright (c) Facebook, Inc. and its affiliates.

module CompactBench where

import Control.DeepSeq
import Control.Exception
import Criterion.Main
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import Data.Default
import Foreign.C
import Foreign.Ptr

import Thrift.Protocol.Compact

import HsTest.Types

simpleObj :: TestStruct
simpleObj = force def

simpleCereal :: ByteString
simpleCereal = force $ serializeCompact simpleObj

complexObj :: TestStruct
complexObj = force $ def
  { testStruct_f_i64 = 0xCCCCCCCC
  , testStruct_f_list = [0..1023]
  }

complexCereal :: ByteString
complexCereal = force $ serializeCompact complexObj

deserialize :: ByteString -> Either String TestStruct
deserialize = deserializeCompact

withCStruct :: ByteString -> (Ptr TestStruct -> IO a) -> IO a
withCStruct cereal f =
  unsafeUseAsCStringLen cereal $ \(buf, len) ->
  bracket (c_deserialize buf (fromIntegral len)) c_delete $ \obj ->
  f obj

main :: IO ()
main =
  withCStruct simpleCereal $ \c_simple ->
  withCStruct complexCereal $ \c_complex -> do
    let
      simple = [ bench "serialize hs" $ nf serializeCompact simpleObj
               , bench "parse hs" $ nf deserialize simpleCereal
               , bench "serialize cpp" $ nfIO $ c_serialize c_simple
               , bench "parse cpp" $ nfIO $ withCStruct simpleCereal (\_ -> pure ())
               ]
      complex = [ bench "serialize hs" $ nf serializeCompact complexObj
                , bench "parse hs" $ nf deserialize complexCereal
                , bench "serialize cpp" $ nfIO $ c_serialize c_complex
                , bench "parse cpp" $ nfIO $ withCStruct complexCereal (\_ -> pure ())
                ]
    defaultMain [ bgroup "simple" simple
                , bgroup "complex" complex
                ]

--------------------------------------------------------------------------------

foreign import ccall unsafe "serialize"
  c_serialize :: Ptr TestStruct -> IO CInt

foreign import ccall unsafe "deserialize"
  c_deserialize :: CString -> CSize -> IO (Ptr TestStruct)

foreign import ccall unsafe "delete_struct"
  c_delete :: Ptr TestStruct -> IO ()
