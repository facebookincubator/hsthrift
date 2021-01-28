-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Lens
  ( lowerCamelCaseLensRules
  , makeLowerCCLenses
  ) where

import Control.Lens
import Data.Char
import Data.List.Split
import Language.Haskell.TH


-- | Rules for makin lower camel case lenses
-- lf_name becomes lfName
lowerCamelCaseLensRules :: LensRules
lowerCamelCaseLensRules = lensRules & lensField .~ lowerCamelCaseNamer
  where
  lowerCamelCase = concat . (_tail %~ map (_head %~ toUpper)) . splitOn "_"
  lowerCamelCaseNamer _ _ n =
    case nameBase n of
      ('_':_) -> []
      field -> [ TopName $ mkName $ lowerCamelCase field ]

-- | Make lower camel case lenses
-- lf_name becomes lfName
makeLowerCCLenses :: Name -> DecsQ
makeLowerCCLenses = makeLensesWith lowerCamelCaseLensRules
