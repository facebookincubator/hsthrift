-- Copyright 2004-present Facebook. All Rights Reserved.
{-# OPTIONS_GHC -fno-warn-unused-imports#-}

module DummyBinaryForBuild
  ( main
  ) where

import A.Types
import A.S.Client
import A.S.Service
import A.ChildService.Client
import A.ChildService.Service
import A.ParentService.Client
import A.ParentService.Service

import B.Types

import C.Types
import C.MyService.Client
import C.MyService.Service

import D.Types

import Namespace.E.Types
import Namespace.E.TU__Service.Client
import Namespace.E.TU__Service.Service

-- Create an empty binary to be used as a build target that depends on the
-- compiled Thrift (so that we can test that the generated Haskell compiles).
main :: IO ()
main = return ()
