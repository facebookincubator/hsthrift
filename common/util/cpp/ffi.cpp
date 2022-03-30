/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cpp/ffi.h"
#include "cpp/wrap.h"

namespace facebook {
namespace hs {
namespace ffi {

const char* outOfMemory = "out of memory";
const char* unknownError = "unknown error";

} // namespace ffi
} // namespace hs
} // namespace facebook

extern "C" {

void hs_ffi_free_error(const char* err) {
  if (err != facebook::hs::ffi::outOfMemory &&
      err != facebook::hs::ffi::unknownError) {
    std::free(const_cast<char*>(err));
  }
}
}
