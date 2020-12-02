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

void get_iobuf_data(IOBuf* iobuf, IOBufData* iobuf_data) {
  iobuf_data->length_ = iobuf->length();
  iobuf_data->data_buf_ = iobuf->data();
  iobuf_data->next_ = iobuf->pop().release();
}

void destroy_iobuf(IOBuf* iobuf, uint8_t* buffer) {
  delete iobuf;
}

} // extern "C"

namespace common {
namespace hs {

std::unique_ptr<folly::IOBuf> newIOBufWrapping(HS_IOBuf* hs_iobuf);

} // namespace hs
} // namespace common
