// Copyright (c) Facebook, Inc. and its affiliates.

#include <cpp/hs_test.h>
#include <if/gen-cpp2/hs_test_types.h>
#include <if/gen-cpp2/hs_test_types_custom_protocol.h>
#include <folly/ExceptionString.h>
#include <folly/io/IOBuf.h>
#include <folly/io/IOBufQueue.h>
#include <glog/logging.h>
#include <thrift/lib/cpp2/protocol/BinaryProtocol.h>
#include <thrift/lib/cpp2/protocol/CompactProtocol.h>
#include <thrift/lib/cpp2/protocol/SimpleJSONProtocol.h>

template <class Reader, class Writer>
size_t echo(const char* data, size_t len, uint8_t** buf) noexcept {
  *buf = nullptr;

  try {
    Reader reader;
    Writer writer;
    cpp2::TestStruct obj;

    std::unique_ptr<folly::IOBuf> readBuf = folly::IOBuf::copyBuffer(data, len);
    reader.setInput(readBuf.get());
    obj.read(&reader);

    size_t outSize = obj.serializedSize(&writer);
    folly::IOBufQueue queue(folly::IOBufQueue::cacheChainLength());
    writer.setOutput(&queue, outSize);

    auto bytes = obj.write(&writer);
    *buf = static_cast<uint8_t*>(malloc(bytes));
    auto pos = *buf;
    for (auto r : *queue.front()) {
      std::memcpy(pos, r.data(), r.size());
      pos += r.size();
    }

    return bytes;
  } catch (std::exception& e) {
    LOG(INFO) << "Failed to deserialize message: " << folly::exceptionStr(e);
    *buf = nullptr;
    return 0;
  }
}

extern "C" {
size_t echoJSON(const char* data, size_t len, uint8_t** buf) noexcept {
  return echo<
      apache::thrift::SimpleJSONProtocolReader,
      apache::thrift::SimpleJSONProtocolWriter>(data, len, buf);
}

size_t echoBinary(const char* data, size_t len, uint8_t** buf) noexcept {
  return echo<
      apache::thrift::BinaryProtocolReader,
      apache::thrift::BinaryProtocolWriter>(data, len, buf);
}

size_t echoCompact(const char* data, size_t len, uint8_t** buf) noexcept {
  return echo<
      apache::thrift::CompactProtocolReader,
      apache::thrift::CompactProtocolWriter>(data, len, buf);
}

size_t echoBinaryCompact(const char* data, size_t len, uint8_t** buf) noexcept {
  return echo<
      apache::thrift::BinaryProtocolReader,
      apache::thrift::CompactProtocolWriter>(data, len, buf);
}

size_t echoCompactBinary(const char* data, size_t len, uint8_t** buf) noexcept {
  return echo<
      apache::thrift::CompactProtocolReader,
      apache::thrift::BinaryProtocolWriter>(data, len, buf);
}
}
