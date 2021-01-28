-- Copyright (c) Facebook, Inc. and its affiliates.

module DynamicTest (main) where

import Control.Exception (bracket)
import Data.Aeson hiding (parseJSON)
import qualified Data.ByteString.Lazy as LB
import Data.Either
import Data.Text (Text)
import Foreign
import System.Timeout (timeout)
import Test.HUnit
import TestRunner

import Foreign.CPP.Marshallable
import Foreign.CPP.Dynamic
import Foreign.CPP.HsStruct

expectedJSON :: Value
expectedJSON = object
  [ "int"    .= (42::Int)
  , "string" .= ("wibble" :: Text)
  , "double" .= (1000.0 / 1024.0 :: Double)
  , "array"  .= [(1::Int)..3]
  , "object" .= object ["a" .= ("b" :: Text)]
  , "null"   .= Null
  , "bool"   .= True
  ]

readDynamicTest :: Test
readDynamicTest = TestCase $ do
  json <- bracket newDynamic delete readDynamic
  assertEqual "readDynamicTest" expectedJSON json

peekHsJSONTest :: Test
peekHsJSONTest = TestCase $ do
  json <- bracket newHsJSON delete $ fmap hsJSON . peek
  assertEqual "peekHsJSONTest" expectedJSON json

withDynamicTest :: Test
withDynamicTest = TestCase $ do
  json <- withDynamic expectedJSON readDynamic
  assertEqual "withDynamicTest" expectedJSON json

nestedValueTest :: Test
nestedValueTest = TestCase $ do
  -- readDynamic should finish in linear time
  Just json <- timeout 1000000 $ withDynamic nestedJSON readDynamic
  assertEqual "nestedValueTest" nestedJSON json
  where
  nestedJSON = foldr ($!) (toJSON True) $
    take 5000 $ cycle [toArray, toObject]
  toArray v = toJSON [v]
  toObject v = object ["key" .= v]

parseJSONTest :: Test
parseJSONTest = TestCase $ do
  result <- parseJSON (LB.toStrict (encode expectedJSON))
  assertBool "parseJSON" $
    case result of
      Left _ -> False
      Right val -> val == expectedJSON

parseJSONError :: Test
parseJSONError = TestCase $ do
  result <- parseJSON "this is not JSON"
  print result
  assertBool "parseJSON" $ isLeft result

main :: IO ()
main = testRunner $ TestList $ map (uncurry TestLabel)
  [ ("readDynamicTest", readDynamicTest)
  , ("peekHsJSONTest", peekHsJSONTest)
  , ("withDynamicTest", withDynamicTest)
  , ("nestedValueTest", nestedValueTest)
  , ("parseJSONTest", parseJSONTest)
  , ("parseJSONError", parseJSONError)
  ]

foreign import ccall unsafe "newDynamic"
  newDynamic :: IO (Ptr Dynamic)

foreign import ccall unsafe "newHsJSON"
  newHsJSON :: IO (Ptr HsJSON)
