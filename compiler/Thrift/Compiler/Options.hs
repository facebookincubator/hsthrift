-- Copyright (c) Facebook, Inc. and its affiliates.

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
  }

data OptLoc = WithoutLoc | WithLoc

data Mode = Lint | EmitJSON OptLoc | EmitCode

data family LangOpts l
