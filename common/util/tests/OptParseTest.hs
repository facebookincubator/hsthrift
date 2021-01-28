-- Copyright (c) Facebook, Inc. and its affiliates.

module OptParseTest where

import Test.HUnit
import TestRunner

import Data.Text (Text)
import Facebook.Init
import Options.Applicative
import Util.OptParse

data Options = Options
  { foo :: Text
  , bar :: Int
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> textOption
    ( long "foo" <> metavar "FOO" <> help "foo" )
  <*> intOption
    ( long "bar" <> value 42 <>  metavar "BAR" <> help "bar" )

optsInfo :: ParserInfo Options
optsInfo = info (helper <*> optionsParser) fullDesc

partialTest :: Test
partialTest = TestLabel "partial" $ TestCase $ do
  doTest -- only defined flags
    [ "--foo=foo", "--bar=7" ]
    []

  doTest -- start with an unknown
    [ "--baz=5", "--foo=foo", "--bar=7" ]
    [ "--baz=5" ]

  doTest -- unknown in the middle
    [ "--foo=foo", "--baz=5", "--bar=7" ]
    [ "--baz=5" ]

  doTest -- end with an unknown
    [ "--foo=foo", "--bar=7", "--baz=5" ]
    [ "--baz=5" ]

  doTest -- intercalate known and unknown
    [ "--snert=4", "--foo=foo", "--quux=wat", "--bar=7", "--baz=5" ]
    [ "--snert=4", "--quux=wat", "--baz=5"]

  doTest -- `--`
    [ "--foo=foo", "--baz=5", "--", "--bar=7", "--" ]
    [ "--baz=5", "--bar=7", "--" ]

  where
    doTest inp ex = do
      (_, args) <- partialParse defaultPrefs optsInfo inp
      assertEqual (show inp) ex args


main :: IO ()
main = withFacebookUnitTest $
  testRunner $ TestList
    [ partialTest
    ]
