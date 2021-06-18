// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include <folly/Optional.h>
#include <optional>

#include "cpp/HsStructDefines.h"
#include "cpp/Marshallable.h"

// HsOption -------------------------------------------------------------------

#define hsc_derive_hs_option_unsafe(cxx_name...)             \
  hsc_printf(                                                \
      "deriveHsOptionUnsafe \"%s\" %lu ",                    \
      #cxx_name,                                             \
      (unsigned long)sizeof(HsOption<hs_option::cxx_name>)); \
  hsc_alignment(HsOption<hs_option::cxx_name>);

#define HS_OPTION_H(Name, Type) \
  namespace hs_option {         \
  using Name = Type;            \
  };

#define HS_OPTION_CPP(Name, Type...)                                       \
  extern "C" void* option_newHsOption##Name(Type* v) noexcept {            \
    return new HsOption<Type>(std::move(*v));                              \
  }                                                                        \
  extern "C" void option_ctorHsOption##Name(void* ret, Type* v) noexcept { \
    new (ret) HsOption<Type>(std::move(*v));                               \
  }                                                                        \
  HS_PEEKABLE(HsOption<Type>);                                             \
  HS_DEFINE_MARSHALLABLE(HsOption##Name, HsOption<Type>);

template <typename T>
HS_STRUCT HsOption {
  T* value = nullptr;
  std::optional<T> opt = std::nullopt;

 public:
  HsOption() {}

  /* implicit */ HsOption(T && value) : opt(std::move(value)) {
    update();
  }

  template <typename U>
  /* implicit */ HsOption(folly::Optional<U> && value) : opt(std::move(value)) {
    update();
  }

  template <typename U>
  /* implicit */ HsOption(std::optional<U> && value) : opt(std::move(value)) {
    update();
  }

  HsOption(const HsOption&) = delete;

  HsOption(HsOption && other) noexcept : opt(std::move(other.opt)) {
    update();
    other.opt = std::nullopt; // a moved optional doesn't empty the value
    other.update();
  }

  HsOption& operator=(const HsOption&) = delete;

  HsOption& operator=(HsOption&& other) noexcept {
    opt = std::move(other.opt);
    update();
    other.opt = std::nullopt; // a moved optional doesn't empty the value
    other.update();
    return *this;
  }

  std::optional<T> toStdOptional()&& {
    std::optional<T> res = std::move(opt);
    opt = std::nullopt;
    update();
    return res;
  }

 private:
  void update() {
    if (opt.has_value()) {
      value = &opt.value();
    } else {
      value = nullptr;
    }
  }
};
