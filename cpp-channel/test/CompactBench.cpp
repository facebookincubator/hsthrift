/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <thrift/lib/cpp2/protocol/Serializer.h>

#include <if/gen-cpp2/hs_test_types.h>

extern "C" {

int serialize(cpp2::TestStruct* obj) noexcept {
  std::string cereal = apache::thrift::CompactSerializer::
      serialize<std::string, cpp2::TestStruct>(*obj);
  return cereal.length();
}

cpp2::TestStruct* deserialize(const uint8_t* buf, size_t len) noexcept {
  return new cpp2::TestStruct(
      apache::thrift::CompactSerializer::deserialize<cpp2::TestStruct>(
          folly::ByteRange(buf, len)));
}

void delete_struct(cpp2::TestStruct* obj) noexcept {
  delete obj;
}
}
