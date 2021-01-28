-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.OptParse
  ( -- * Options
    intOption
  , doubleOption
  , textOption
  , maybeStrOption
  , maybeTextOption
  , maybeIntOption
  , textCommaSplit
  , testCommaSplitAndStrip
  , stringCommaSplit
  , readCommaSplit
  , absFilePathOption
  , relativeFilePathOption
  , maybeAbsFilePathOption
  , maybeRelativeFilePathOption
  , jsonOption
    -- * Commands
  , commandParser
    -- * Pure Parser
  , runParserOnString
    -- * Dragons and stuff
  , partialParse
  ) where

import Data.Aeson (FromJSON(..), eitherDecodeStrict')
import Data.Bifunctor (second)
import Data.Char (isSpace)
import qualified Data.List.Split as List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative
import Options.Applicative.Types

import Util.FilePath
import Util.String


intOption :: Mod OptionFields Int -> Parser Int
intOption = option auto

doubleOption :: Mod OptionFields Double -> Parser Double
doubleOption = option auto

textOption :: Mod OptionFields Text -> Parser Text
textOption = option (Text.pack <$> str)

maybeStrOption :: Mod OptionFields String -> Parser (Maybe String)
maybeStrOption = optional . option str

maybeTextOption :: Mod OptionFields Text -> Parser (Maybe Text)
maybeTextOption = optional . option (Text.pack <$> str)

maybeIntOption :: Mod OptionFields Int -> Parser (Maybe Int)
maybeIntOption = optional . option (read <$> str)

textCommaSplit :: ReadM [Text]
textCommaSplit = Text.splitOn "," . Text.pack <$> str

testCommaSplitAndStrip :: ReadM [Text]
testCommaSplitAndStrip = map Text.strip <$> textCommaSplit

stringCommaSplit :: ReadM [String]
stringCommaSplit = List.splitOn "," <$> str

readCommaSplit :: Read a => ReadM [a]
readCommaSplit = map (read . strip) <$> stringCommaSplit

jsonOption :: FromJSON a => Mod OptionFields a -> Parser a
jsonOption =
  option (eitherReader $ eitherDecodeStrict' . encodeUtf8 . Text.pack)

-- | Returns an absolute file path, while handling relative paths and ~
absFilePathOption :: DirEnv -> Mod OptionFields FilePath -> Parser FilePath
absFilePathOption e = option (absolutiseWith e <$> str)

-- | Returns a relative file path
relativeFilePathOption :: Mod OptionFields FilePath -> Parser FilePath
relativeFilePathOption = option str


-- | Returns an absolute file path, while handling relative paths and ~
maybeAbsFilePathOption
  :: DirEnv
  -> Mod OptionFields FilePath
  -> Parser (Maybe FilePath)
maybeAbsFilePathOption e = optional . option (absolutiseWith e <$> str)

-- | Returns a relative file path
maybeRelativeFilePathOption
  :: Mod OptionFields FilePath
  -> Parser (Maybe FilePath)
maybeRelativeFilePathOption = optional . option str

-- | Build a command subparser from a name, description, and parser.
--
-- Example: commandParser "foo" (progDesc "Foo does bar.") (... parser ...)
commandParser
  :: String     -- ^ command name (the 'cmd' in 'prog cmd --cmdflag1')
  -> InfoMod a  -- ^ command description in --help output
  -> Parser a   -- ^ parser of command's flags
  -> Parser a
commandParser cmd desc p =
  subparser $ metavar cmd <> command cmd (info (helper <*> p) desc)

-- | Run given parser on a string.
runParserOnString :: String -> Parser a -> String -> Either String a
runParserOnString cmdName p args =
  case parse of
    Success x -> Right x
    Failure f -> Left $ fst $ renderFailure f cmdName
    CompletionInvoked _ -> Left "Completion Invoked"
  where
    parse = execParserPure
      (prefs mempty) (info (p <**> helper) fullDesc) (quotedWords args)
    recurse (w,s) = w : quotedWords s
    -- Mimic shell's ability to group tokens with double quotes.
    quotedWords s =
      case dropWhile isSpace s of
        "" -> []
        ('"':cs) -> recurse . second tail $ break (=='"') cs
        s' -> recurse $ break isSpace s'

-- -----------------------------------------------------------------------------
-- Things from Options.Applicative that have changed

-- | Attempts to parse across *all* arguments, returning ones that were
-- unrecognized. Fails if a required argument is missing.
partialParse :: ParserPrefs -> ParserInfo a -> [String] -> IO (a, [String])
partialParse pprefs pinfo args = handleParseResult res
  where
    pinfo' = pinfo
      { infoParser = (,) <$> infoParser pinfo <*> many (strArgument mempty)
      , infoPolicy = ForwardOptions
      }
    res = execParserPure pprefs pinfo' args
