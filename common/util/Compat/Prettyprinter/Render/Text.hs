{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}
-- | Just a compat layer to avoid CPP when using prettyprinter
{-# LANGUAGE CPP #-}
module Compat.Prettyprinter.Render.Text (module P) where

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter.Render.Text as P
#else
import Data.Text.Prettyprint.Doc.Render.Text as P
#endif
