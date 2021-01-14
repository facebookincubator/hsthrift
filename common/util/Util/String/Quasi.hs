-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TemplateHaskell #-}
module Util.String.Quasi (s) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.String

-- | An easy way to write multiline string literals using QuasiQuotes. Useful
-- for e.g. large JSON literals.
--
-- >  [s| this
-- >    is a
-- >    multi-line string literal with \no escaping\
-- >  |]
--
s :: QuasiQuoter
s = QuasiQuoter {
    quoteExp  = \str -> return $ AppE (VarE 'fromString) $ LitE $ StringL str,
    quotePat  = \_ -> fail "quotePat",
    quoteType = \_ -> fail "quoteType",
    quoteDec  = \_ -> fail "quoteDec"
}
