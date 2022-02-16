// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include "cpp/HsStruct.h"

namespace facebook::common::hs {

HS_STRUCT Nonmovable {
  int64_t resource;
  HsString description;

 public:
  Nonmovable(int64_t resource, std::string && description)
      : resource(resource), description(std::move(description)) {}

  ~Nonmovable() {
    // free_resource(resource);
  }

  Nonmovable(const Nonmovable&) = delete;
  Nonmovable(Nonmovable &&) = delete;
  Nonmovable& operator=(const Nonmovable&) = delete;
  Nonmovable& operator=(Nonmovable&&) = delete;
};

} // namespace facebook::common::hs

HS_STD_VARIANT_H(MyCppVariant, int32_t, HsString, HsOption<HsJSON>);
HS_OPTION_H(MyCppVariant, hs_std_variant::MyCppVariant);
