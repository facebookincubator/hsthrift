{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}

module Util.HSE
  (
    classA
  ) where

import Language.Haskell.Exts.Syntax hiding (Type)
import qualified Language.Haskell.Exts.Syntax as HS

classA :: l -> QName l -> [HS.Type l] -> Asst l
#if MIN_VERSION_haskell_src_exts(1,22,0)
classA l conName args = TypeA l (foldl (TyApp l) (TyCon l conName) args)
#else
classA = ClassA
#endif
