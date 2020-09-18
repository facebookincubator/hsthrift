-- Copyright 2014-present Facebook. All Rights Reserved.


module BinaryTest (main) where

import Test.HUnit
import TestRunner

import Data.Aeson hiding (decode, encode)
import Data.Binary
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Text (Text)

import HsPrefix.Types
import Thrift.Protocol (ThriftSerializable)
import Util.Binary

assertCompatible
  :: (Binary a, Eq a, Show a) => (a -> Put) -> Get a -> a -> Assertion
assertCompatible putX getX x = do
  assertEqual "compatible get" x $ runGet getX (encode x)
  assertEqual "compatible put" x $ decode (runPut (putX x))
  assertEqual "equivalent" (encode x) $ runPut (putX x)

assertInvertible :: (Eq a, Show a) => (a -> Put) -> Get a -> a -> Assertion
assertInvertible putX getX x =
  assertEqual "invertible" x $ runGet getX (runPut (putX x))

eitherTest :: Test
eitherTest = TestLabel "putEither/getEither" $ TestList
  [ t (Left 3 :: Either Int Int)
  , t (Right 9 :: Either Int Int)
  , t (Left False :: Either Bool Text)
  , t (Right "True" :: Either Bool Text)
  ]
  where
  t :: (Binary a, Eq a, Show a, Binary b, Eq b, Show b) => Either a b -> Test
  t x = TestLabel (show x) . TestCase $ do
    assertCompatible (putEither put put) (getEither get get) x
    assertInvertible (putEither put put) (getEither get get) x

jsonTest :: Test
jsonTest = TestLabel "putJSON/getJSON" $ TestList
  [ t ()
  , t (233 :: Int)
  , t ("test" :: Text)
  , t (object ["x" .= False, "y" .= [0::Int .. 9]] :: Value)
  ]
  where
  t :: (Eq a, Show a, FromJSON a, ToJSON a) => a -> Test
  t x = TestLabel (show x) . TestCase $
    assertInvertible putJSON getJSON x

thriftTest :: Test
thriftTest = TestLabel "putThrift/getThrift" $ TestList
  [ t s
  , t ps
  , t $ U_A E_A
  , t $ U_B s
  , t $ PU_A PE_B
  , t $ PU_B ps
  ]
  where
  s = S { s_A = 10, s_B = E_B }
  ps = PrefixedS { ps_A = -1, ps_B = PE_A }
  t :: (Eq a, Show a, ThriftSerializable a) => a -> Test
  t x = TestLabel (show x) . TestCase $
    assertInvertible putThrift getThrift x


main :: IO ()
main = testRunner $ TestList
  [ eitherTest
  , jsonTest
  , thriftTest
  ]
