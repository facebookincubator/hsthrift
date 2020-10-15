#pragma once

#include <folly/io/IOBuf.h>

extern "C" {

struct HS_IOBuf {
  uint8_t** str_arr;
  size_t* len_arr;
  size_t len;
};
}

namespace common {
namespace hs {

std::unique_ptr<folly::IOBuf> newIOBufWrapping(HS_IOBuf* hs_iobuf);
}
} // namespace common
