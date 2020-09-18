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

tests :: Test
tests = TestLabel "IOBufTest" $ TestList [ roundTripTest ]

main :: IO ()
main = testRunner tests

--------------------------------------------------------------------------------

foreign import ccall "echo"
  c_echo :: Ptr IOBuf -> IO CString
