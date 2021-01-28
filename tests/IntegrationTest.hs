-- Copyright (c) Facebook, Inc. and its affiliates.

module IntegrationTest where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString, packCStringLen, useAsCStringLen)
import qualified Data.ByteString.Char8 as ByteString
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Exit
import Test.QuickCheck as QC

import HsTest.Types
import Thrift.Protocol.Binary
import Thrift.Protocol.Compact
import Thrift.Protocol.JSON

-- Serialize a struct from Haskell, send it to C++ where it gets deserialized
-- and then re-serialized and then read it back into Haskell.
prop_roundtrip
  :: (TestStruct -> ByteString)
  -> (ByteString -> Either String TestStruct)
  -> (CString -> CSize -> Ptr CString -> IO CSize)
  -> TestStruct
  -> Property
prop_roundtrip serialize deserialize echo struct = ioProperty $
  useAsCStringLen (serialize struct) $ \(str, len) ->
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
         let result = deserialize cereal == Right struct
         unless result $ do
           ByteString.putStrLn $ serialize struct
           ByteString.putStrLn cereal
           print result
         return result)
--         return $ deserialize cereal == Right struct)

main :: IO ()
main = do
  result <- mapM quickCheckResult
            [ prop_roundtrip serializeJSON deserializeJSON c_echoJSON
            , prop_roundtrip serializePrettyJSON deserializeJSON c_echoJSON
            , prop_roundtrip serializeBinary deserializeBinary c_echoBinary
            , prop_roundtrip serializeCompact deserializeCompact c_echoCompact
            ]
  if all success result then exitSuccess else exitFailure
    where
      success QC.Success{} = True
      success _ = False

--------------------------------------------------------------------------------

foreign import ccall unsafe "echoJSON"
  c_echoJSON :: CString -> CSize -> Ptr CString -> IO CSize

foreign import ccall unsafe "echoBinary"
  c_echoBinary :: CString -> CSize -> Ptr CString -> IO CSize

foreign import ccall unsafe "echoCompact"
  c_echoCompact :: CString -> CSize -> Ptr CString -> IO CSize
