-- Copyright (c) Facebook, Inc. and its affiliates.

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
