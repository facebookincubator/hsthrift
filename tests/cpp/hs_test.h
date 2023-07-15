/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstdint>

#include <sys/types.h>

extern "C" {
size_t echoJSON(const char* data, size_t len, uint8_t** buf) noexcept;
size_t echoBinary(const char* data, size_t len, uint8_t** buf) noexcept;
size_t echoCompact(const char* data, size_t len, uint8_t** buf) noexcept;
size_t echoBinaryCompact(const char* data, size_t len, uint8_t** buf) noexcept;
size_t echoCompactBinary(const char* data, size_t len, uint8_t** buf) noexcept;
}
