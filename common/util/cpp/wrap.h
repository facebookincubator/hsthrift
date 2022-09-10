/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <cstring>
#include <stdexcept>

namespace facebook {
namespace hs {
namespace ffi {

template <typename F>
const char* wrap(F&& f) noexcept {
  // The '\1' prefix instructs the marshaller in FFI.hs to not free those
  // strings. The prefix itself will be stripped out. This is only really
  // necessary specifically for the outOfMemory message where we might not be
  // able to allocate a new message string - although arguably, we might just
  // want to abort in such a case.
  static const char *outOfMemory = "\1out of memory";
  static const char *unknownError = "\1unknown error";
  try {
    f();
    return nullptr;
  } catch (const std::exception& e) {
    const char* s = strdup(e.what());
    return s == nullptr ? outOfMemory : s;
  } catch (...) {
    return unknownError;
  }
}

template <typename F>
void wrap_(F&& f) noexcept {
  try {
    f();
  } catch (...) {
  }
}

template <typename T>
void free_(T* obj) noexcept {
  wrap_([=] { delete obj; });
}

} // namespace ffi
} // namespace hs
} // namespace facebook
