-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TemplateHaskell #-}

module HsStructTestTypes
  ( Nonmovable(..)
  , MyVariant(..)
  ) where

import Data.ByteString (ByteString)
import Data.Int

import Foreign
import Foreign.C.Types
import Foreign.CPP.HsStruct
import Foreign.CPP.HsStruct.HsOption
import Foreign.CPP.HsStruct.HsStdVariant
import Foreign.CPP.Marshallable.TH

#include <cpp/HsStdVariant.h>
#include <hsc.h>
#include <tests/HsStructHelper.h>

#{verbatim using facebook::common::hs::Nonmovable;}

data Nonmovable = Nonmovable Int ByteString
  deriving (Eq, Show)

instance Addressable Nonmovable

instance Storable Nonmovable where
  sizeOf _ = #{size HsMaybe<Nonmovable>}
  alignment _ = #{alignment HsMaybe<Nonmovable>}
  poke = error "Nonmovable: poke not implemented"
  peek p = do
    resource <- #{peek Nonmovable, resource} p
    description <- #{peek Nonmovable, description} p
    return $ Nonmovable resource $ hsByteString description

$(deriveDestructibleUnsafe "HsMaybeNonmovable" [t| HsMaybe Nonmovable |])

data MyVariant
  = I Int32
  | S HsByteString
  | J (HsOption HsJSON)

$(#{derive_hs_std_variant_unsafe MyCppVariant} "MyVariant" [t| MyVariant |])
$(#{derive_hs_option_unsafe MyCppVariant} [t| MyVariant |])
