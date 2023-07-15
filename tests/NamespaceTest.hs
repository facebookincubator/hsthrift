{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module NamespaceTest where

import Thrift.Test.Namespace.Types as Namespace
import Thrift.Test.Internal.NamespaceIncluded.Types as NamespaceIncluded

main :: IO ()
main = do
  print $ let x = Namespace.X 0 :: Namespace.X in x
  print $ let y = NamespaceIncluded.X 1 :: Namespace.Y in y
