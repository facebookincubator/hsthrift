{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.Monoid
  ( mwhen
  , munless
  ) where


-- | Use the supplied value if the condition is 'True',
-- return 'mempty' otherwise. This is like 'guard', but for
-- 'Monoid'.
mwhen :: Monoid m => Bool -> m -> m
mwhen cond v = if cond then v else mempty

-- | The opposite of `mwhen`.
munless :: Monoid m => Bool -> m -> m
munless = mwhen . not
