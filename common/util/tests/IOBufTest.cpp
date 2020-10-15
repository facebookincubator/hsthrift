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
}
