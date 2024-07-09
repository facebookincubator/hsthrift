/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/json/dynamic.h>

using Dynamic = folly::dynamic;

enum DType { tNull, tArray, tBool, tDouble, tInt64, tObject, tString };

union DValue {
  void* null;
  int boolean;
  size_t size; /* array, object */
  double doubl;
  int64_t int64;
  const char* string;
};

namespace facebook {
namespace hs {

void readDynamic(const Dynamic* d, DType* ty, DValue* val) noexcept;

int readDynamicArray(
    const Dynamic* d,
    size_t size,
    const Dynamic** elems) noexcept;
int readDynamicObject(
    const Dynamic* d,
    size_t size,
    const Dynamic** keys,
    const Dynamic** vals) noexcept;

void createDynamic(Dynamic* ret, DType ty, DValue* val) noexcept;
void createDynamicArray(Dynamic* ret, size_t size, Dynamic* elems) noexcept;
void createDynamicObject(
    Dynamic* ret,
    size_t size,
    const char** keys,
    Dynamic* vals) noexcept;

const folly::dynamic*
parseJSON(const char* str, int64_t len, char** err) noexcept;

} // namespace hs
} // namespace facebook
