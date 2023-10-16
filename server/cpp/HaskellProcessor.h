/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <memory>
#include <unordered_set>

#include <folly/MapUtil.h>
#include <folly/Memory.h>
#include <folly/io/IOBuf.h>
#include <folly/io/async/EventBase.h>

#include <thrift/lib/cpp2/async/AsyncProcessor.h>

namespace apache {
namespace thrift {

struct TResponse {
  uint8_t* data;
  size_t len;
  std::vector<std::pair<std::string, std::string>> headers = {};
};

// Equivalent of ProcessorCallback from CppServer.hs
// using TCallback = uint8_t* (*)(uint16_t, const uint8_t*, size_t, size_t*);
using TCallback = void (*)(uint16_t, const uint8_t*, size_t, TResponse*);

/**
 * The core connection piece between CPP ThriftServers and how to process the
 * bytes. This gets called with the set of bytes we received off the wire.
 * It gets called *after* the Header stuff has been pulled out.
 */
class HaskellAsyncProcessor : public AsyncProcessor {
 public:
  HaskellAsyncProcessor(
      TCallback callback,
      AsyncProcessorFactory::MethodMetadataMap& metadataMap);

  void processSerializedCompressedRequestWithMetadata(
      apache::thrift::ResponseChannelRequest::UniquePtr req,
      apache::thrift::SerializedCompressedRequest&& serializedCompressedRequest,
      const apache::thrift::AsyncProcessorFactory::MethodMetadata&
          methodMetadata,
      apache::thrift::protocol::PROTOCOL_TYPES protType,
      apache::thrift::Cpp2RequestContext* context,
      folly::EventBase* eb,
      apache::thrift::concurrency::ThreadManager* tm) override;

  void executeRequest(
      ServerRequest&& request,
      const AsyncProcessorFactory::MethodMetadata& methodMetadata) override;

 protected:
  TCallback callback_;
  AsyncProcessorFactory::MethodMetadataMap& metadataMap_;

 private:
  void run(
      apache::thrift::ResponseChannelRequest::UniquePtr req,
      apache::thrift::LegacySerializedRequest&& legacySerializedRequest,
      apache::thrift::Cpp2RequestContext* context,
      folly::EventBase* eb,
      TCallback cb,
      bool oneway,
      bool fromExecuteRequest);
};

class HaskellAsyncProcessorFactory : public AsyncProcessorFactory {
 public:
  explicit HaskellAsyncProcessorFactory(
      TCallback callback,
      AsyncProcessorFactory::MethodMetadataMap& metadataMap)
      : callback_(callback), metadataMap_(metadataMap) {}

  std::unique_ptr<AsyncProcessor> getProcessor() override {
    return std::make_unique<HaskellAsyncProcessor>(callback_, metadataMap_);
  }

  CreateMethodMetadataResult createMethodMetadata() override {
    WildcardMethodMetadataMap wildcardMap;
    wildcardMap.wildcardMetadata = std::make_shared<WildcardMethodMetadata>(
        MethodMetadata::ExecutorType::ANY);
    wildcardMap.knownMethods = metadataMap_;

    return wildcardMap;
  }

  // TODO(T89004867): Call onStartServing() and onStopServing() hooks for
  // non-C++ thrift servers
  std::vector<ServiceHandlerBase*> getServiceHandlers() override {
    return {};
  }

 private:
  TCallback callback_;
  AsyncProcessorFactory::MethodMetadataMap& metadataMap_;
};

} // namespace thrift
} // namespace apache
