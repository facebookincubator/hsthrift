// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include "cpp/HsStruct.h"

HS_STD_VARIANT_H(MyCppVariant, int32_t, HsString, HsOption<HsJSON>);
HS_OPTION_H(MyCppVariant, hs_std_variant::MyCppVariant);
