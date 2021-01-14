// Copyright (c) Facebook, Inc. and its affiliates.

#include <gflags/gflags.h>

#include "cpp/HsStruct.h"

using namespace gflags;

extern "C" int facebook_gflags_hs_set_flag_default(
    char const* const name,
    char const* const value) noexcept {
  auto const ret = SetCommandLineOptionWithMode(
      name, value, FlagSettingMode::SET_FLAGS_DEFAULT);
  return !ret.empty();
}

extern "C" int facebook_gflags_hs_set_flag_value(
    char const* const name,
    char const* const value) noexcept {
  auto const ret = SetCommandLineOptionWithMode(
      name, value, FlagSettingMode::SET_FLAGS_VALUE);
  return !ret.empty();
}

extern "C" int facebook_gflags_hs_set_flag_value_if_default(
    char const* const name,
    char const* const value) noexcept {
  auto const ret = SetCommandLineOptionWithMode(
      name, value, FlagSettingMode::SET_FLAG_IF_DEFAULT);
  return !ret.empty();
}

extern "C" int facebook_gflags_hs_get_flag_value(
    char const* const name,
    HsString* const out) noexcept {
  std::string outs;
  auto const ret = GetCommandLineOption(name, &outs);
  *out = std::move(outs);
  return ret;
}

extern "C" FlagSaver* facebook_gflags_hs_flag_saver_create() noexcept {
  return new FlagSaver();
}

extern "C" void facebook_gflags_hs_flag_saver_delete(
    FlagSaver* const obj) noexcept {
  delete obj;
}

extern "C" HsString* facebook_gflags_hs_hs_string_create() noexcept {
  return new HsString();
}

extern "C" void facebook_gflags_hs_hs_string_delete(
    HsString* const obj) noexcept {
  delete obj;
}
