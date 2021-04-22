// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include <memory>
#include <unordered_set>

#include <folly/Memory.h>
#include <folly/io/IOBuf.h>
#include <folly/io/async/EventBase.h>

#include <thrift/lib/cpp2/async/AsyncProcessor.h>

namespace apache {
namespace thrift {

struct TResponse {
  uint8_t* data;
  size_t len;
  char* ex_name = nullptr;
  size_t ex_name_len = 0;
  char* ex_text = nullptr;
  size_t ex_text_len = 0;
  bool client_error = false;
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
      const std::unordered_set<std::string>& oneways);

  void processSerializedRequest(
      ResponseChannelRequest::UniquePtr req,
      apache::thrift::SerializedRequest&& serializedRequest,
      protocol::PROTOCOL_TYPES protType,
      Cpp2RequestContext* context,
      folly::EventBase* eb,
      concurrency::ThreadManager* tm) override;

 protected:
  virtual folly::Func funcFromTask(std::shared_ptr<EventTask> task) {
    return folly::Func([task = std::move(task)] { task->run(); });
  }

  TCallback callback_;
  const std::unordered_set<std::string>& oneways_;
};

class HaskellAsyncProcessorFactory : public AsyncProcessorFactory {
 public:
  explicit HaskellAsyncProcessorFactory(
      TCallback callback,
      const std::unordered_set<std::string>& oneways)
      : callback_(callback), oneways_(oneways) {}

  std::unique_ptr<AsyncProcessor> getProcessor() override {
    return std::make_unique<HaskellAsyncProcessor>(callback_, oneways_);
  }

  // TODO(T89004867): Call onStartServing() and onStopServing() hooks for
  // non-C++ thrift servers
  std::vector<ServiceHandler*> getServiceHandlers() override {
    return {};
  }

 private:
  TCallback callback_;
  const std::unordered_set<std::string>& oneways_;
};

} // namespace thrift
} // namespace apache
