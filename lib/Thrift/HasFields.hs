{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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
