{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.Show
  ( Pretty(..)
  , showBrace
  ) where

-- | Class of human-readable things. Whereas:
--
-- > read (show x) == x
--
-- Humans don't care. Uses 'String' and 'ShowS' for now; if this
-- becomes problematic, it can use 'Text' and 'Builder'.
class Pretty a where
  prettyPrec :: Int -> a -> ShowS
  prettyPrec _ x s = pretty x ++ s
  pretty :: a -> String
  pretty x = prettyPrec 0 x ""

showBrace :: ShowS -> ShowS
showBrace s = showString "{ " . s . showString " }"
