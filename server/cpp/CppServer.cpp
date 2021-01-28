// Copyright (c) Facebook, Inc. and its affiliates.

#include "cpp/CppServer.h"

#include <memory>
#include <unordered_set>

#include <folly/CppAttributes.h>
#include <folly/Memory.h>
#include <folly/io/IOBuf.h>
#include <folly/io/async/EventBase.h>

#include <thrift/lib/cpp/TApplicationException.h>
#include <thrift/lib/cpp2/async/AsyncProcessor.h>
#include <thrift/lib/cpp2/server/ThriftServer.h>

#include "cpp/Destructible.h"
#include "cpp/HsStruct.h"

namespace apache {
namespace thrift {

/**
 * Listener for events coming out of the server.
 * Used to notify when the server has started serving traffic.
 */
class CppEventHandler : public server::TServerEventHandler {
 public:
  CppEventHandler(
      int fd,
      int* port,
      ThriftServer& server,
      std::shared_ptr<TServerEventHandler> prevHandler)
      : fd_(fd), port_(port), server_(server), prevEventHandler_(prevHandler) {}

  void preServe(const folly::SocketAddress* address) override {
    server_.setServerEventHandler(prevEventHandler_);
    if (prevEventHandler_) {
      prevEventHandler_->preServe(address);
    }
    *port_ = address->getPort();
    notify();
  }

  void handleServeError(const std::exception& x) override {
    server_.setServerEventHandler(prevEventHandler_);
    if (prevEventHandler_) {
      prevEventHandler_->handleServeError(x);
    }
    *port_ = 0;
    notify();
  }

 private:
  int fd_;
  int* port_;
  ThriftServer& server_;
  std::shared_ptr<TServerEventHandler> prevEventHandler_;

  void notify() {
    int64_t buf = 1;
    write(fd_, &buf, 8);
  }
};

class CppServer : public ThriftServer {
 public:
  explicit CppServer(
      TCallback callback,
      TFactory factoryFn,
      int desiredPort,
      std::unordered_set<std::string>&& oneways)
      : oneways_(std::move(oneways)) {
    setPort(desiredPort);
    setProcessorFactory(
        std::unique_ptr<AsyncProcessorFactory>(factoryFn(callback, oneways_)));
  }

  // Start serving traffic
  HsString* FOLLY_NULLABLE
  go(int fd, int* portPtr, void (*modify)(ThriftServer&)) {
    try {
      auto prevHandler = getEventHandler();
      eHandler_ =
          std::make_shared<CppEventHandler>(fd, portPtr, *this, prevHandler);
      setServerEventHandler(eHandler_);

      // invoke optional callback to modify the ThriftServer
      if (modify) {
        modify(*this);
      }

      // Thrift main loop.  This will run indefinitely, until stop() is called
      serve();
    } catch (const std::exception& e) {
      *portPtr = 0;
      auto exStr = e.what();
      LOG(ERROR) << "Unable to start serving traffic: " << exStr;

      getEventHandler()->handleServeError(e);

      return new HsString(exStr);
    }
    return nullptr;
  }

 private:
  std::shared_ptr<CppEventHandler> eHandler_;
  std::unordered_set<std::string> oneways_;
};
} // namespace thrift
} // namespace apache

// ****************************************************
// Exported functions

extern "C" {

apache::thrift::AsyncProcessorFactory* c_haskell_factory(
    apache::thrift::TCallback callback,
    const std::unordered_set<std::string>& oneways) noexcept {
  return new apache::thrift::HaskellAsyncProcessorFactory(callback, oneways);
}

using CreateCppServerResult = HsEither<apache::thrift::CppServer*, HsString>;

HS_DEFINE_DESTRUCTIBLE(
    CreateCppServerResult,
    HsEither<apache::thrift::CppServer*, HsString>);

CreateCppServerResult* c_create_cpp_server(
    apache::thrift::TCallback callback,
    apache::thrift::TFactory factoryFn,
    int desiredPort,
    const char** onewayNames,
    size_t* onewaySizes,
    size_t onewayLength) noexcept {
  try {
    std::unordered_set<std::string> oneways;
    for (size_t i = 0; i < onewayLength; i++) {
      oneways.emplace(std::string(onewayNames[i], onewaySizes[i]));
    }

    auto cppServer = new apache::thrift::CppServer(
        callback, factoryFn, desiredPort, std::move(oneways));
    return new CreateCppServerResult(HsLeft, std::move(cppServer));
  } catch (const std::exception& e) {
    auto exStr = e.what();
    LOG(ERROR) << "Failed to start server: " << exStr;

    return new CreateCppServerResult(HsRight, exStr);
  }
}

void c_destroy_cpp_server(apache::thrift::CppServer* s) noexcept {
  delete s;
}

HsString* FOLLY_NULLABLE c_serve_cpp_server(
    apache::thrift::CppServer* s,
    int fd,
    int* portPtr,
    void (*modify)(apache::thrift::ThriftServer&)) noexcept {
  return s->go(fd, portPtr, modify);
}

void c_stop_cpp_server(apache::thrift::CppServer* s) noexcept {
  try {
    s->stop();
  } catch (const std::exception& e) {
    LOG(ERROR) << "Error stopping server: " << e.what();
  }
}
}
