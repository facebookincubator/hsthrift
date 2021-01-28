// Copyright (c) Facebook, Inc. and its affiliates.

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
