-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsStructTestTypes
  ( Nonmovable(..)
  , OnlyMovable(..)
  , MyVariant(..)
  ) where

import Data.ByteString (ByteString)
import Data.Int

import Foreign
import Foreign.C.Types
import Foreign.CPP.HsStruct
import Foreign.CPP.HsStruct.HsOption
import Foreign.CPP.HsStruct.HsStdTuple
import Foreign.CPP.HsStruct.HsStdVariant
import Foreign.CPP.Marshallable.TH

#include <cpp/HsStdTuple.h>
#include <cpp/HsStdVariant.h>
#include <hsc.h>
#include <tests/HsStructHelper.h>

#{verbatim using facebook::common::hs::OnlyMovable;}
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

data OnlyMovable = OnlyMovable Int
  deriving (Eq, Show)

instance Addressable OnlyMovable

instance Storable OnlyMovable where
  sizeOf _ = #{size OnlyMovable}
  alignment _ = #{alignment OnlyMovable}
  poke p (OnlyMovable r) = #{poke OnlyMovable, r_} p r
  peek p = do
    resource <- #{peek OnlyMovable, r_} p
    return $ OnlyMovable resource


data MyVariant
  = I Int32
  | S HsByteString
  | J (HsOption HsJSON)

-- Marshallable first
$(deriveMarshallableUnsafe "HsOptionMyCppVariant" [t| HsOption MyVariant |])
$(deriveMarshallableUnsafe "MyCppVariant" [t| MyVariant |])
$(deriveMarshallableUnsafe "CppTupleIntJSONOnlyMovable" [t| HsStdTuple (Int32, HsJSON, OnlyMovable, HsEither HsText Int) |])
$(deriveMarshallableUnsafe "TupleStringString" [t| HsStdTuple (HsText, HsText) |])
$(deriveMarshallableUnsafe "HsOptionTupleStringString" [t| HsOption (HsStdTuple (HsText, HsText)) |])

-- Constructions next
$(#{derive_hs_std_variant_unsafe MyCppVariant} "MyVariant" [t| MyVariant |])
$(#{derive_hs_option_unsafe MyCppVariant} [t| MyVariant |])
$(#{derive_hs_std_tuple_unsafe CppTupleIntJSONOnlyMovable} [t| (Int32, HsJSON, OnlyMovable, HsEither HsText Int) |])
$(#{derive_hs_std_tuple_unsafe TupleStringString} [t| (HsText, HsText) |])
$(#{derive_hs_option_unsafe TupleStringString} [t| HsStdTuple (HsText, HsText) |])
