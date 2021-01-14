-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Linter
  ( LintError(..)
  , Severity(..)
  , LintFormat(..)
  , parseLintFormat
  , reportLintErrors
  ) where

import Data.Aeson (ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics
import Options.Applicative
import TextShow

import Util.Text

-- Lint Error Types ------------------------------------------------------------

data LintError = LintError
  { source_file :: FilePath
  , source_line :: Int
  , source_col  :: Int
  , severity    :: Severity
  , message     :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON LintError

data Severity = Warning | Error
  deriving (Show, Eq, Generic)

instance ToJSON Severity where
  toJSON Warning = Aeson.String "warning"
  toJSON Error = Aeson.String "error"

-- Options and Parsers ---------------------------------------------------------

data LintFormat
  = HumanReadable
  | JsonDump

parseLintFormat :: Parser LintFormat
parseLintFormat = flag HumanReadable JsonDump $ mconcat
  [ long "json"
  , help "Dump output as JSON"
  ]

-- Outputs ---------------------------------------------------------------------

reportLintErrors :: LintFormat -> [LintError] -> IO ()
reportLintErrors HumanReadable errors
  | null errors = Text.putStrLn "No errors."
  | otherwise = mapM_ (Text.putStrLn . renderLintError) errors
reportLintErrors JsonDump errors = ByteString.putStrLn $ Aeson.encode errors

renderLintError :: LintError -> Text
renderLintError LintError{..} = Text.unlines $
  colorize
  (Text.pack source_file <> ":" <> showt source_line <> ":" <>
   showt source_col <> ": " <> Text.pack (show severity)) :
  wrapText 2 80 message
  where
    colorize x = case severity of
      -- Warning -> Yellow
      Warning -> "\ESC[33m" <> x <> "\ESC[0m"
      -- Error -> Red
      Error   -> "\ESC[31m" <> x <> "\ESC[0m"
