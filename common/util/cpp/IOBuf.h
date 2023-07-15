/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/io/IOBuf.h>

extern "C" {

struct HS_IOBuf {
  uint8_t** str_arr;
  size_t* len_arr;
  size_t len;
};

using folly::IOBuf;

struct IOBufData {
  int length_;
  const uint8_t* data_buf_;
  IOBuf* next_;
};

void get_iobuf_data(IOBuf* iobuf, IOBufData* iobuf_data);

void destroy_iobuf(IOBuf* iobuf, uint8_t* buffer);

} // extern "C"

namespace common {
namespace hs {

std::unique_ptr<folly::IOBuf> newIOBufWrapping(HS_IOBuf* hs_iobuf);

} // namespace hs
} // namespace common
