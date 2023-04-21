// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include <folly/Preprocessor.h>
#include "cpp/HsStruct.h"

namespace facebook::common::hs {

HS_STRUCT OnlyMovable {
  int64_t r_;

 public:
  OnlyMovable() = delete;
  explicit OnlyMovable(int64_t r) : r_(r) {}

  OnlyMovable(const OnlyMovable&) = delete;
  OnlyMovable& operator=(const OnlyMovable&) = delete;

  OnlyMovable(OnlyMovable &&) = default;
  OnlyMovable& operator=(OnlyMovable&&) = delete;
};

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
HS_STD_TUPLE_H(
    CppTupleIntJSONOnlyMovable,
    FB_SINGLE_ARG(
        int32_t,
        HsJSON,
        facebook::common::hs::OnlyMovable,
        HsEither<HsString, int64_t>));
HS_STD_TUPLE_H(TupleStringString, FB_SINGLE_ARG(HsString, HsString));
HS_OPTION_H(TupleStringString, FB_SINGLE_ARG(HsStdTuple<HsString, HsString>));
