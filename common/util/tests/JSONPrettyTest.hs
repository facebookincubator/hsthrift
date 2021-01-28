-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE QuasiQuotes #-}
module JSONPrettyTest (main) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text.Prettyprint.Doc as Pretty hiding ((<>))
import Text.JSON

import Test.HUnit
import TestRunner

import Util.JSON.Pretty ()
import Util.String.Quasi

jsonString :: String
jsonString = [s|{"byt":33,"nat":42,"array_of_byte":"eHl6AAB4ZmY","array_of_nat":[99,98],"record_":{"a":34,"b":35},"sum_":{"d":36},"named_record_":{"alpha":1,"beta":{"wed":true}},"named_sum_":{"tue":37},"named_enum_":2,"pred":{"id":3},"maybe_":{},"bool_":true,"string_":"Hello\u0000world!\u0000","null_":null}|]

jsonPrettyTest :: Test
jsonPrettyTest = TestLabel "jsonPrettyTest" $ TestCase $ do
  json <- case Text.JSON.decode jsonString of
    Error _ -> assertFailure "parse"
    Ok (v :: JSValue) -> return v
  let
    prettyJson = show $ pretty json
    Just v0 = Aeson.decode (B.pack jsonString) :: Maybe Aeson.Value
    Just v1 = Aeson.decode (B.pack prettyJson)
  assertEqual "pretty" v0 v1

main :: IO ()
main = testRunner $ TestList
  [ jsonPrettyTest
  ]
