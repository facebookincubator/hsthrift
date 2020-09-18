module Main where

-- async
import qualified StreamTest

-- datastruct
import qualified MovingAverageRateLimiterTest
import qualified RateLimiterMapTest

-- util
import qualified IOBufTest
import qualified AllocLimitTest
import qualified UnitTests
import qualified RWVarTest
import qualified THTest
import qualified FilePathTest
-- import qualified BinaryTest
--   ^^^ disabled because requires fb-util
import qualified OptParseTest
import qualified LensTest
import qualified ToExpTest
-- import qualified LogTest
--   ^^^ no real test in there
-- import qualified GFlagsTest
import qualified AesonTest
import qualified BufferTest
import qualified ExceptionTest
import qualified ControlExceptionTest
import qualified JSONPrettyTest
import qualified IOTest
import qualified TimeSecTest
import qualified ListTest
import qualified EncodingTest
import qualified ConcurrentTest
import qualified MD5Test
import qualified Control.MonadTest
import qualified StringQuasiTest

import Test.Hspec (hspec)
import Test.HUnit
import TestRunner

main :: IO ()
main = do
  testRunner $ TestLabel "fb-util-tests" $ TestList
    [ StreamTest.tests
    , MovingAverageRateLimiterTest.tests
    , RateLimiterMapTest.tests
    , IOBufTest.tests
    , AllocLimitTest.tests
    , UnitTests.tests
    , RWVarTest.tests
    , THTest.tests
    , FilePathTest.tests
    , OptParseTest.tests
    , LensTest.tests
    , ToExpTest.tests
    , AesonTest.tests
    , BufferTest.tests
    , EncodingTest.tests
    , ExceptionTest.tests
    , ControlExceptionTest.tests
    , JSONPrettyTest.tests
    , IOTest.tests
    , TimeSecTest.tests
    , ListTest.tests
    , ConcurrentTest.tests
    , MD5Test.tests
    , Control.MonadTest.tests
    , StringQuasiTest.tests
    ]

  -- TODO: hspec GFlagsTest.spec
  -- BinaryTest.main    -- "disabled": requires thrift-lib
  -- LogTest.main       -- "disabled": doesn't contain any hunit test,
                        -- just IO actions (logging)
