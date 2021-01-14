-- Copyright (c) Facebook, Inc. and its affiliates.

{-# OPTIONS -Wno-orphans #-}

-- | Display JSON values using pretty printing combinators.
--
-- Adapted from Text.Pretty.JSON in the json package, which is
--   Copyright (c) Galois, Inc. 2007
--
-- Why would you use this instead of Aeson + aeson-pretty?
--   1. You want to retain the ordering of fields in JSON objects. Aeson
--      doesn't do this.
--   2. The pretty-printer here produces more compact output than aeson-pretty.

module Util.JSON.Pretty
  ( -- instance Pretty JSValue
  ) where

import Data.Text.Prettyprint.Doc as Pretty
import Data.Ratio
import Data.Char
import Numeric
import Text.JSON

instance Pretty JSValue where
  pretty v = case v of
    JSNull -> "null"
    JSBool True -> "true"
    JSBool False -> "false"
    JSRational asf x -> pp_number asf x
    JSString x -> pp_string (fromJSString x)
    JSArray vs -> pp_array vs
    JSObject xs -> pp_object (fromJSObject xs)

pp_number :: Bool -> Rational -> Doc a
pp_number _ x | denominator x == 1 = pretty (numerator x)
pp_number True x = pretty (fromRational x :: Float)
pp_number _ x = pretty (fromRational x :: Double)

pp_array :: [JSValue] -> Doc a
pp_array xs = sep [nest 2 (vsep ("[" : punctuate comma (map pretty xs))), "]"]

pp_object :: [(String,JSValue)] -> Doc a
pp_object xs =
  sep [nest 2 (vsep ("{" : punctuate comma (map pp_field xs))), "}"]
  where pp_field (k,v) = pp_string k <> colon <+> pretty v

pp_string :: String -> Doc a
pp_string x = dquotes $ hcat $ map pp_char x
  where pp_char '\\'            = "\\\\"
        pp_char '"'             = "\\\""
        pp_char c | isControl c = uni_esc c
        pp_char c               = pretty [c]

        uni_esc c = "\\u" <> pretty (pad 4 (showHex (fromEnum c) ""))

        pad n cs  | len < n   = replicate (n-len) '0' ++ cs
                  | otherwise = cs
          where len = length cs
