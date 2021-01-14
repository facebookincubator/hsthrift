-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Thrift.HasFields (HasField(..)) where

import GHC.TypeLits

-- | HasField type family. Same as
-- GHC.Records.HasField, but custom so we can overlap it
class HasField (x :: Symbol) r a | x r -> a where
  getField :: r -> a
