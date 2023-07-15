/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <boost/mp11.hpp>
#include <glog/logging.h>
#include <variant>

#include "cpp/Marshallable.h"

namespace hs_std_variant::detail {

template <typename V>
void std_variant_poke(void* var, void* val, uint32_t idx) {
  constexpr auto variant_size = std::variant_size_v<V>;
  DCHECK_NE(idx, 0);
  DCHECK_LT(idx, variant_size);
  boost::mp11::mp_with_index<variant_size>(idx, [&](auto tag) {
    using type = std::variant_alternative_t<tag, V>;
    auto& val_ = *reinterpret_cast<type*>(val);
    new (var) V{std::in_place_index<tag>, std::move(val_)};
  });
}

} // namespace hs_std_variant::detail

// ****************************************************************************

#define hsc_derive_hs_std_variant_unsafe(cxx_name...)   \
  hsc_printf(                                           \
      "deriveHsStdVariantUnsafe \"%s\" %lu ",           \
      #cxx_name,                                        \
      (unsigned long)sizeof(hs_std_variant::cxx_name)); \
  hsc_alignment(hs_std_variant::cxx_name);

#define HS_STD_VARIANT_H(Name, ...)                       \
  namespace hs_std_variant {                              \
  using Name = std::variant<std::monostate, __VA_ARGS__>; \
  }

#define HS_STD_VARIANT_CPP(Name)                                              \
  HS_PEEKABLE(hs_std_variant::Name);                                          \
  HS_DEFINE_MARSHALLABLE(Name, hs_std_variant::Name)                          \
                                                                              \
  extern "C" void* std_variant_peek##Name(void* var, int32_t* idx) noexcept { \
    constexpr auto variant_size = std::variant_size_v<hs_std_variant::Name>;  \
    auto& var_ = *reinterpret_cast<hs_std_variant::Name*>(var);               \
    *idx = var_.index();                                                      \
    DCHECK_GT(*idx, 0);                                                       \
    DCHECK_LT(*idx, variant_size);                                            \
    return boost::mp11::mp_with_index<variant_size>(*idx, [&](auto tag) {     \
      return reinterpret_cast<void*>(&std::get<tag>(var_));                   \
    });                                                                       \
  }                                                                           \
                                                                              \
  extern "C" void std_variant_poke##Name(                                     \
      void* variant, void* val, int32_t tag) noexcept {                       \
    hs_std_variant::detail::std_variant_poke<hs_std_variant::Name>(           \
        variant, val, tag);                                                   \
  }
