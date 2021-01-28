// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include <folly/io/IOBufQueue.h>
#include <thrift/lib/cpp2/protocol/BinaryProtocol.h>
#include <thrift/lib/cpp2/protocol/SimpleJSONProtocol.h>
#include <string>

// ***********************************************************************
// Serialization

template <class P, class T>
std::string serializeThrift(const T& f) {
  folly::IOBufQueue queue;
  P protocol;
  protocol.setOutput(&queue);
  std::string out;

  f.write(&protocol);
  queue.appendToString(out);
  return out;
}

template <class T>
std::string serializeJSON(const T& f) {
  return serializeThrift<apache::thrift::SimpleJSONProtocolWriter, T>(f);
}

template <class T>
std::string serializeBinary(const T& f) {
  return serializeThrift<apache::thrift::BinaryProtocolWriter, T>(f);
}

// ***********************************************************************
// Deserialization

template <class P, class T>
T deserializeThrift(const std::string& output) {
  auto buf = folly::IOBuf::copyBuffer(output);
  P protReader;
  protReader.setInput(buf.get());
  T f;
  f.read(&protReader);
  return f;
}

template <class T>
T deserializeJSON(const std::string& output) {
  return deserializeThrift<apache::thrift::SimpleJSONProtocolReader, T>(output);
}

template <class T>
T deserializeBinary(const std::string& output) {
  return deserializeThrift<apache::thrift::BinaryProtocolReader, T>(output);
}
