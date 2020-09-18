-- @lint-ignore HLINT1 T25377293 Grandfathered in
instance Prelude.Bounded HsInclude where
  minBound = hsInclude2_get Prelude.undefined
  maxBound = hsconst
