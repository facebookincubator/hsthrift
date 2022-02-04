-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TemplateHaskell #-}

module HsStructTestTypes
  ( MyVariant(..)
  ) where

import Data.Int

import Foreign
import Foreign.C.Types
import Foreign.CPP.HsStruct
import Foreign.CPP.HsStruct.HsOption
import Foreign.CPP.HsStruct.HsStdVariant
import Foreign.CPP.Marshallable.TH

#include <cpp/HsStdVariant.h>
#include <tests/HsStructHelper.h>

data MyVariant
  = I Int32
  | S HsByteString
  | J (HsOption HsJSON)

$(#{derive_hs_std_variant_unsafe MyCppVariant} "MyVariant" [t| MyVariant |])
$(#{derive_hs_option_unsafe MyCppVariant} [t| MyVariant |])
