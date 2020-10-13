{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Thrift.HasFields (HasField(..)) where

-- | HasField type family. Same as
-- GHC.Records.HasField, but custom so we can overlap it
class HasField (x :: k) r a | x r -> a where
  getField :: r -> a
