/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

namespace {

// Constructs object on already allocated memory.
template <typename T>
void constructImpl(T* p) noexcept {
  new (p) T;
}

// This allocates memory, and then constructs object.
template <typename T>
T* newImpl() noexcept {
  return new T;
}

} // namespace

#ifndef HS_DEFINE_DEFAULT_CONSTRUCTIBLE
#define HS_DEFINE_DEFAULT_CONSTRUCTIBLE(Name, Type...)                     \
  static_assert(std::is_default_constructible_v<Type>);                    \
  extern "C" void constructible_constructDefault##Name(Type* p) noexcept { \
    constructImpl(p);                                                      \
  }                                                                        \
  extern "C" Type* constructible_newDefault##Name() noexcept {             \
    return newImpl<Type>();                                                \
  }
#endif
