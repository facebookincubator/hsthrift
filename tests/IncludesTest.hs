{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module IncludesTest where

import qualified A.Types as A
import qualified B.Types as B
import qualified C.Types as C
import qualified D.Types as D
import qualified E.Types as E

-- If this compiles, then the test has passed
main :: IO ()
main = print a
  where
    a = A.A b c
    b = B.B d e B.B2_X
    c = C.C e
    d = D.D 0
    e = E.E ":)"
