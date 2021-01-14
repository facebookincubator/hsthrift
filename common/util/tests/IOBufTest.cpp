// Copyright (c) Facebook, Inc. and its affiliates.

#include <cpp/IOBuf.h>
#include <folly/io/IOBuf.h>

extern "C" {

uint8_t* echo(HS_IOBuf* hs_iobuf) noexcept {
  auto ioBuf = common::hs::newIOBufWrapping(hs_iobuf);

  auto str = ioBuf->moveToFbString();
  auto len = str.length();

  // This will get free'd by the haskell garbage collector
  auto buf = static_cast<uint8_t*>(malloc(len + 1));
  memcpy((void*)buf, (void*)str.c_str(), len);
  buf[len] = 0; // Null terminator

  return buf;
}

IOBuf* create_buffer() noexcept {
  const char* const strs[] = {
      "All happy families are alike; ",
      "every unhappy family is unhappy in its own way."};
  const int lens[] = {30, 47};
  std::unique_ptr<IOBuf> bufs[2];
  for (size_t i = 0; i < 2; i++) {
    bufs[i] = IOBuf::createCombined(50);
    strncpy((char*)bufs[i]->writableData(), strs[i], lens[i]);
    bufs[i]->append(lens[i]);
    if (i)
      bufs[0]->appendChain(std::move(bufs[i]));
  }
  return bufs[0].release();
}
}
