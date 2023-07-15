{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module AddPrefixTest where

import Test.HUnit
import TestRunner

import Data.Default (def)
import HsPrefix.Types

toPrefixedE :: E -> PrefixedE
toPrefixedE E_A = PE_A
toPrefixedE E_B = PE_B

toPrefixedS :: S -> PrefixedS
toPrefixedS S{..} = PrefixedS
  { ps_A = s_A
  , ps_B = toPrefixedE s_B
  }

toPrefixedU :: U -> PrefixedU
toPrefixedU u = case u of
  U_A a -> PU_A $ toPrefixedE a
  U_B b -> PU_B $ toPrefixedS b
  U_EMPTY -> PU_EMPTY

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "def" $ def ~?= toPrefixedU def
  ]
