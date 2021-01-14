-- Copyright (c) Facebook, Inc. and its affiliates.

module IOBufTest where

import Data.ByteString.Lazy as Lazy
import Data.ByteString.Unsafe
import Foreign.C
import Foreign.Ptr
import Util.IOBuf
import TestRunner
import Test.HUnit

lazyBS :: Lazy.ByteString
lazyBS = fromChunks ["hello", " ", "world"]

roundTripTest :: Test
roundTripTest = TestLabel "round trip" $ TestCase $ do
  bs <- unsafePackMallocCString =<< unsafeWithIOBuf lazyBS c_echo
  assertEqual "echo'd" "hello world" bs

-- `show` adds quotes
testString :: String
testString = "\"All happy families are alike; " ++
 "every unhappy family is unhappy in its own way.\""

fromCTest :: Test
fromCTest = TestLabel "from C" $ TestCase $ do
  x <- toLazy c_create
  assertEqual "marshalled" testString $ show x

main :: IO ()
main = testRunner $ TestList [ roundTripTest, fromCTest ]

--------------------------------------------------------------------------------

foreign import ccall "echo"
  c_echo :: Ptr IOBuf -> IO CString
foreign import ccall unsafe "create_buffer" c_create :: Ptr IOBuf
