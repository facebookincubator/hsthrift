-- Copyright 2004-present Facebook. All Rights Reserved.
{-# LANGUAGE CPP #-}

module Util.HSE
  (
    classA
  ) where

import Language.Haskell.Exts.Syntax hiding (Type)
import qualified Language.Haskell.Exts.Syntax as HS

classA :: l -> QName l -> [HS.Type l] -> Asst l
#if __GLASGOW_HASKELL__ > 804
classA l conName args = TypeA l (foldl (TyApp l) (TyCon l conName) args)
#else
classA = ClassA
#endif
