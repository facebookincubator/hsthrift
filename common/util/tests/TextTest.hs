{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module TextTest (main) where

import qualified Data.ByteString as B
import Data.Word
import qualified Data.Text as Text
import Text.Printf
import Test.QuickCheck
import Test.HUnit
import TestRunner

import Util.Text

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "hexToBytes" $ TestCase $ do
      result <- quickCheckResult prop_hexToBytes
      case result of
        Success{} -> return ()
        _ -> assertFailure "failed"
  ]

prop_hexToBytes :: [Word8] -> Property
prop_hexToBytes bytes =
  not (null bytes) ==>
    Right (B.pack bytes) == hexToBytes (Text.pack (showHex bytes))
  where
  showHex [] = []
  showHex (x:rest) = printf "%x" x <> concatMap (printf "%02x") rest
