{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Thrift.Compiler.Plugins.Linter
  ( Linter
  , LangOpts(..)
  ) where

import Thrift.Compiler.Options
import Thrift.Compiler.Plugin

-- Linter Types ----------------------------------------------------------------

data Linter

-- Linter Options -------------------------------------------------------------

data instance LangOpts Linter = NoOpts

-- Type Class Instance ---------------------------------------------------------

instance Typecheckable Linter where
  type Interface Linter = ()

  -- These are all vacuously empty because there is no data family instance for
  -- SpecialType Linter
  typecheckSpecialConst ty _ = case ty of
  qualifySpecialType _ ty = case ty of
  eqSpecial ty _ =  case ty of

  backTranslateType ty = case ty of
  backTranslateLiteral ty _ = case ty of
