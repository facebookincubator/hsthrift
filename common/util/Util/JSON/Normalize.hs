{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.JSON.Normalize (
    normalize,
  ) where

import Data.List
import Text.JSON as JSON

-- | Sort all the objects. Useful for deterministic test output when
-- using Aeson < 2; later versions of Aeson provide ordered keys so
-- this isn't necessary.
normalize :: JSValue -> JSValue
normalize j = case j of
  JSON.JSNull -> j
  JSON.JSBool{} -> j
  JSON.JSRational{} -> j
  JSON.JSString{} -> j
  JSON.JSArray v -> JSON.JSArray $ sort $ map normalize v
  JSON.JSObject o -> JSON.JSObject $ JSON.toJSObject $
    sort $ map (fmap normalize) $ JSON.fromJSObject o
