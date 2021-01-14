// Copyright (c) Facebook, Inc. and its affiliates.

#include <folly/io/IOBuf.h>

#include <cpp/IOBuf.h>

extern "C" {

void marshal_bytestring(uint8_t* buf, size_t len) noexcept {
  auto ioBuf = folly::IOBuf::wrapBuffer(buf, len);
  return;
}

void marshal_iobuf(HS_IOBuf* hs_iobuf) noexcept {
  auto ioBuf = common::hs::newIOBufWrapping(hs_iobuf);
  return;
}
}
