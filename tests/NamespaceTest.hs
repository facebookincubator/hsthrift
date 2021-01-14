-- Copyright (c) Facebook, Inc. and its affiliates.

module NamespaceTest where

import Thrift.Test.Namespace.Types as Namespace
import Thrift.Test.Internal.NamespaceIncluded.Types as NamespaceIncluded

main :: IO ()
main = do
  print $ let x = Namespace.X 0 :: Namespace.X in x
  print $ let y = NamespaceIncluded.X 1 :: Namespace.Y in y
