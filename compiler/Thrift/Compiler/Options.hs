{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Thrift.Compiler.Options
  ( Options(..), defaultOptions
  , Mode(..), LangOpts
  , OptLoc(..)
  ) where

import Data.Text (Text)

data Options l = Options
  { optsPath          :: FilePath
  , optsOutPath       :: FilePath
  , optsIncludePath   :: FilePath
  , optsThriftMade    :: Maybe FilePath
  , optsGenMode       :: Mode
  , optsLangSpecific  :: LangOpts l
  , optsRecursive     :: Bool
  , optsReqSymbols    :: Maybe [Text]
  , optsSingleOutput  :: Bool
  , optsLenient       :: Bool -- ^ whether to accept older weirder thrift files
  , optsLenientStillGenCode :: Bool -- ^ whether to still generate code under lenient
  }

defaultOptions :: LangOpts l -> Options l
defaultOptions opts = Options
  { optsPath          = ""
  , optsOutPath       = "."
  , optsIncludePath   = "."
  , optsThriftMade    = Nothing
  , optsGenMode       = Lint
  , optsLangSpecific  = opts
  , optsRecursive     = False
  , optsReqSymbols    = Nothing
  , optsSingleOutput  = False
  , optsLenient       = False
  , optsLenientStillGenCode = False
  }

data OptLoc = WithoutLoc | WithLoc

data Mode = Lint | EmitJSON OptLoc | EmitCode

data family LangOpts l
