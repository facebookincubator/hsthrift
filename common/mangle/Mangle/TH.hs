-- Copyright (c) Facebook, Inc. and its affiliates.

module Mangle.TH ( mangle ) where

import qualified Mangle
import Language.Haskell.TH

mangle :: String -> Q [Dec] -> Q [Dec]
mangle sig decls = do
  mangled <- case Mangle.mangle sig of
    Left err -> fail (show err)
    Right mangled -> return mangled
  let
    mangleForeign (ForeignD (ImportF conv safety _ name ty)) = do
      return $ ForeignD (ImportF conv safety mangled name ty)
    mangleForeign other = return other
  mapM mangleForeign =<< decls
