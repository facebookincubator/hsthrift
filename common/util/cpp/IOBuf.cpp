// Copyright (c) Facebook, Inc. and its affiliates.

#include <cpp/IOBuf.h>

std::unique_ptr<folly::IOBuf> common::hs::newIOBufWrapping(HS_IOBuf* hs_iobuf) {
  auto ioBuf = folly::IOBuf::wrapBuffer(
      hs_iobuf->str_arr[hs_iobuf->len - 1],
      hs_iobuf->len_arr[hs_iobuf->len - 1]);
  for (int i = hs_iobuf->len - 2; i >= 0; i--) {
    auto last = std::move(ioBuf);
    ioBuf =
        folly::IOBuf::wrapBuffer(hs_iobuf->str_arr[i], hs_iobuf->len_arr[i]);
    ioBuf->appendChain(std::move(last));
  }

  return ioBuf;
}

extern "C" {

void get_iobuf_data(IOBuf* iobuf, IOBufData* iobuf_data) {
  iobuf_data->length_ = iobuf->length();
  iobuf_data->data_buf_ = iobuf->data();
  iobuf_data->next_ = iobuf->pop().release();
}

void destroy_iobuf(IOBuf* iobuf, uint8_t* buffer) {
  delete iobuf;
}
}
