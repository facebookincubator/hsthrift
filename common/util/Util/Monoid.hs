-- Copyright (c) Facebook, Inc. and its affiliates.

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
