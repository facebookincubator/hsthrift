/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/json.h>
#include "cpp/HsStruct.h"

extern "C" {

const folly::dynamic* newDynamic() noexcept;

const HsJSON* newHsJSON() noexcept;

void initializeJson(const char* filename) noexcept;
const char* getJsonAsCStringLen(int64_t* len) noexcept;
const folly::dynamic* getJsonAsPtrDynamic() noexcept;
const HsJSON* getJsonAsPtrHsJSON() noexcept;

const folly::dynamic* parseJSON(const char* str, int64_t len) noexcept;
int64_t compareDynamic(
    const folly::dynamic* lhs,
    const folly::dynamic* rhs) noexcept;
}
