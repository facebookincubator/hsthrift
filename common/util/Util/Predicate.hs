-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Predicate
  ( Pred
  , predAnd
  , predTrue
  , predFalse
  ) where

import Control.Applicative (liftA2)

-- | Predicate function.
type Pred a = a -> Bool

-- | Combine two predicate functions to produce a new function that holds if
-- both input predicates hold.
predAnd :: Pred a -> Pred a -> Pred a
predAnd = liftA2 (&&)

-- | Predicate which returns True for all inputs
predTrue :: Pred a
predTrue _ = True

-- | Predicate which returns False for all inputs
predFalse :: Pred a
predFalse _ = False
