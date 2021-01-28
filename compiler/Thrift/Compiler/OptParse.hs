-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE CPP #-}
module Thrift.Compiler.OptParse
  ( optionsParser
  , ThriftLanguage
  , SomeOptions(..), SomeLangOpts(..)
  ) where

#if __GLASGOW_HASKELL__ <= 804
import Data.Monoid ( (<>) )
#endif
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import Options.Applicative

import Thrift.Compiler.Options
import Thrift.Compiler.Plugin
import Thrift.Compiler.Plugins.Haskell
import Thrift.Compiler.Plugins.Linter

-- Types -----------------------------------------------------------------------

type ThriftLanguage l =
  ( Typecheckable l
  , Typeable l
  )

data SomeOptions = forall l. ThriftLanguage l => TheseOptions (Options l)

-- Hack because data families must be saturated
data SomeLangOpts = forall l. ThriftLanguage l => TheseLangOpts (LangOpts l)

-- Parsers ---------------------------------------------------------------------

optionsParser :: Parser SomeOptions
optionsParser = do
  optsPath <- strArgument $ mconcat
    [ metavar "INPUT"
    , help "Generate code for thrift file INPUT"
    ]
  optsOutPath <- strOption $ mconcat
    [ long "output-dir"
    , short 'o'
    , metavar "DIR"
    , help "Output generated files in DIR"
    , value "."
    ]
  optsIncludePath <- strOption $ mconcat
    [ long "include-dir"
    , short 'I'
    , metavar "DIR"
    , help "Base directory for includes DIR"
    , value "."
    ]
  optsThriftMade <- maybeStr
    [ long "record-genfiles"
    , metavar "GENFILES"
    , help "Output generated file to GENFILES"
    ]
  optsGenMode <- modeParser
  lo <- langOptsParser
  optsReqSymbols <- maybeList
    [ long "required-symbols"
    , help "List of symbols that should be generated"
    ]
  optsRecursive <- switch $ mconcat
    [ long "recursive"
    , short 'r'
    , help "Recursively generate dependencies"
    ]
  optsSingleOutput <- switch $ mconcat
    [ long "single-output"
    , help "When emitting JSON in recursive mode, dump output to a single file"
    ]
  optsLenient <- switch $ mconcat
    [ long "lenient"
    , help "Parse & Typecheck weird old thrift files, disables code generation"
    ]
  -- We can support parsing these options for compatibility, but they don't do
  -- anything
  _ <- switch (long "allow-64bit-consts")
  _ <- switch (long "allow-neg-keys")
  _ <- switch (long "allow-neg-enum-vals")
  _ <- maybeStr [long "templates"]
  pure $ case lo of
    TheseLangOpts optsLangSpecific -> TheseOptions Options{..}

modeParser :: Parser Mode
modeParser =
  flag' Lint
  (mconcat
   [ long "lint"
   , help "Only invoke the typechecker, do not emit any code"
   ]) <|>
  flag' (EmitJSON WithoutLoc)
  (mconcat
   [ long "emit-json"
   , help "Dump the typechecked thrift AST as JSON in the output directory"
   ]) <|>
  flag' (EmitJSON WithLoc)
  (mconcat
   [ long "emit-json-loc"
   , help $ "Dump the typechecked thrift AST as JSON"
            <> " with Locations in the output directory"
   ]) <|>
  -- Default Mode is code generation
  pure EmitCode

langOptsParser :: Parser SomeLangOpts
langOptsParser =
  (TheseLangOpts <$> hsOptsParser) <|>
  pure (TheseLangOpts NoOpts)

hsOptsParser :: Parser (LangOpts Haskell)
hsOptsParser = do
  _ <- flag' () $ mconcat
    [ long "hs"
    , help "Target Haskell"
    ]
  hsoptsEnableHaddock <- switch $ mconcat
    [ long "enable-haddock"
    , help "Enable generating and building haddock"
    ]
  hsoptsUseInt <- switch $ mconcat
    [ long "use-int"
    , help $ "Use the Haskell type Int for the thrift type i64 " ++
             "(not recommended for 32-bit systems)"
    ]
  hsoptsUseHashMap <- switch $ mconcat
    [ long "use-hash-map"
    , help "Use the Haskell type HashMap for the thrift type map"
    ]
  hsoptsUseHashSet <- switch $ mconcat
    [ long "use-hash-set"
    , help "Use the Haskell type HashSet for the thrift type set"
    ]
  hsoptsDupNames <- switch $ mconcat
    [ long "duplicate-names"
    , help "Do not de-dup struct field names"
    ]
  hsoptsExtensions <- listOption
    [ long "extensions"
    , help "List of language extensions to turn on"
    , value []
    ]
  hsoptsGenPrefix <- strOption $ mconcat
    [ long "gen-prefix"
    , short 'g'
    , metavar "DIR"
    , help "Prefix for generated file paths, default 'gen-hs2'"
    , value "gen-hs2"
    ]
  hsoptsExtraHasFields <- switch $ mconcat
    [ long "extra-hasfields"
    , help "Generate HasField instances for unqualified struct names"
    ]

  return HsOpts{..}

-- Helpers ---------------------------------------------------------------------

maybeStr :: [Mod OptionFields FilePath] -> Parser (Maybe FilePath)
maybeStr = optional . strOption . mconcat

strToList :: String -> [Text]
strToList s | null s    = []
            | otherwise = Text.splitOn "," $ Text.pack s

listOption :: [Mod OptionFields String] -> Parser [Text]
listOption = fmap strToList . strOption . mconcat

maybeList :: [Mod OptionFields String] -> Parser (Maybe [Text])
maybeList = optional . listOption
