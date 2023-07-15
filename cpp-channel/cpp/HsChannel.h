/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/io/IOBuf.h>
#include <thrift/lib/cpp/concurrency/Thread.h>
#include <thrift/lib/cpp/protocol/TProtocolTypes.h>
#include <thrift/lib/cpp2/async/RequestChannel.h>

#include <HsFFI.h>

enum Status { SEND_ERROR, SEND_SUCCESS, RECV_ERROR, RECV_SUCCESS };

struct FinishedRequest {
  const uint8_t* buffer;
  size_t len;
  Status status;
};

using InnerChannel = std::unique_ptr<
    apache::thrift::RequestChannel,
    folly::DelayedDestruction::Destructor>;

class HsCallback : public apache::thrift::RequestClientCallback {
 public:
  explicit HsCallback(
      std::shared_ptr<InnerChannel> client,
      int cap,
      HsStablePtr send_mvar,
      HsStablePtr recv_mvar,
      FinishedRequest* send_result,
      FinishedRequest* recv_result)
      : client_(std::move(client)),
        cap_(cap),
        send_mvar_(send_mvar),
        recv_mvar_(recv_mvar),
        send_result_(send_result),
        recv_result_(recv_result) {}

  bool isInlineSafe() const override {
    // our callbacks do memcpy/malloc/hs_try_putmvar (which is nonblocking and
    // very quick).
    // this should be inline safe
    return true;
  }

  // Note [onResponse leak]
  //
  // The memory containing the result is transferred from C++ to
  // Haskell in onResponseError / onResponse.  hs_try_putmvar() wakes
  // up the Haskell thread running CppChannel.hsc:sendCollector
  // or CppChannel.hsc:recvCollector, which takes ownership of the
  // memory in a ForeignPtr.
  //
  // This is carefully designed to not leak even if the thread making
  // the original Thrift request is
  // interrupted. sendCollector/recvCollector are running in a
  // separate thread which will run and take ownership of the memory
  // even if the original thread that made the request has gone away.
  //
  // However, if the program exits after onResponse/onResponseError
  // but before the Haskell thread running sendCollector/recvCollector
  // runs, this memory may be detected as a leak by leak-checkers such
  // as ASAN. It's not really a leak, just an artifact of exiting at
  // the wrong time.
  //
  void onResponseError(folly::exception_wrapper ew) noexcept override {
    auto ex = ew.what();
    size_t len = ex.length();
    auto buf = std::unique_ptr<uint8_t, decltype(free)*>{
        reinterpret_cast<uint8_t*>(malloc(len * sizeof(uint8_t))), free};
    // If you get a memory leak here, see Note [onResponse leak].
    std::memcpy(buf.get(), ex.data(), len);

    bool sendError = false;
    ew.with_exception(
        [&](apache::thrift::transport::TTransportException const& tex) {
          sendError = tex.getType() ==
              apache::thrift::transport::TTransportException::NOT_OPEN;
        });
    if (sendError || !recv_result_) {
      send_result_->status = SEND_ERROR;
      send_result_->buffer = buf.release();
      send_result_->len = len;
      hs_try_putmvar(cap_, send_mvar_);
    } else {
      requestSentHelper();
      recv_result_->status = RECV_ERROR;
      recv_result_->buffer = buf.release();
      recv_result_->len = len;
      hs_try_putmvar(cap_, recv_mvar_);
    }
    delete this;
  }

  void onResponse(
      apache::thrift::ClientReceiveState&& state) noexcept override {
    SCOPE_EXIT {
      delete this;
    };
    requestSentHelper();
    if (!recv_result_) {
      return;
    }

    if (state.isException()) {
      auto ex = state.exception().what();
      size_t len = ex.length();
      auto buf = std::unique_ptr<uint8_t, decltype(free)*>{
          reinterpret_cast<uint8_t*>(malloc(len * sizeof(uint8_t))), free};
      // If you get a memory leak here, see Note [onResponse leak].
      std::memcpy(buf.get(), ex.data(), len);

      recv_result_->status = RECV_ERROR;
      recv_result_->buffer = buf.release();
      recv_result_->len = len;
      hs_try_putmvar(cap_, recv_mvar_);
    } else {
      auto ioBuf = apache::thrift::LegacySerializedResponse(
                       state.protocolId(),
                       0,
                       state.messageType(),
                       methodName_,
                       state.extractSerializedResponse())
                       .buffer;
      size_t len = ioBuf->computeChainDataLength();
      auto msg = std::unique_ptr<uint8_t, decltype(free)*>{
          reinterpret_cast<uint8_t*>(malloc(len * sizeof(uint8_t))), free};

      auto pos = msg.get();
      for (auto r : *ioBuf) {
        std::memcpy(pos, r.data(), r.size());
        pos += r.size();
      }

      recv_result_->status = RECV_SUCCESS;
      recv_result_->buffer = msg.release();
      recv_result_->len = len;
      hs_try_putmvar(cap_, recv_mvar_);
    }
  }

  void setMethodName(std::string name) {
    methodName_ = std::move(name);
  }

  std::string const& getMethodName() const {
    return methodName_;
  }

  void setContextStack(apache::thrift::ContextStack::UniquePtr&& contextStack) {
    contextStack_ = std::move(contextStack);
  }

 private:
  void requestSentHelper() {
    send_result_->status = SEND_SUCCESS;
    hs_try_putmvar(cap_, send_mvar_);
  }

  std::shared_ptr<InnerChannel> client_; // see Note [channel lifetime]
  int cap_;
  HsStablePtr send_mvar_;
  HsStablePtr recv_mvar_;
  FinishedRequest* send_result_;
  FinishedRequest* recv_result_;
  std::string methodName_;

  // Note that the contextStack_ *need* to be declared after
  // methodName_. ContextStack contains a pointer reference
  // to the method-name, and needs to access this pointer in its
  // destructor. Fields are destructed in reverse order, and we need
  // methodName_ to have a larger lifetime than the ContextStack
  // object.
  apache::thrift::ContextStack::UniquePtr contextStack_;
};

using CallbackPtr = std::unique_ptr<
    HsCallback,
    apache::thrift::RequestClientCallback::RequestClientCallbackDeleter>;

/* Note [channel lifetime]
 *
 * The ChannelWrapper implementation keeps the InnerChannel alive
 * until all the outstanding HsCallbacks have been called. This is to
 * support use cases that need to receive the responses to requests
 * outside of the scope of the channel creation
 * (e.g. withHeaderChannel).  An example is the Haxl datasource for
 * Thrift services, which does not have a way to scope
 * withHeaderChannel over the lifetime of the requests. Without this
 * feature, the channel is closed on exit from the scope of
 * withHeaderChannel, and the outstanding requests will fail.
 *
 * The alternative to keeping the InnerChannel alive here would be to
 * make the client do its own reference counting, which is hard and
 * error-prone.
 *
 * For a test case see thrift/cpp-channel/tests/LifetimeTest.hs
 */

class ChannelWrapper : public apache::thrift::TClientBase {
 public:
  explicit ChannelWrapper(InnerChannel client)
      : client_(std::make_shared<InnerChannel>(std::move(client))) {}

  ~ChannelWrapper() {
    auto evb = client_->get()->getEventBase();
    // Only run the destructor once all callbacks are done
    auto destroyClient = [client = std::move(client_)]() mutable {};
    // Move the unique_ptr into the lambda so that it gets destructed in the
    // eventbase thread
    if (evb) {
      evb->runInEventBaseThread(std::move(destroyClient));
    }
  }

  apache::thrift::protocol::PROTOCOL_TYPES getProtocolType(uint8_t buf) {
    // Check hex value to see which protocol is being used
    return buf == 0x82
        ? apache::thrift::protocol::PROTOCOL_TYPES::T_COMPACT_PROTOCOL
        : apache::thrift::protocol::PROTOCOL_TYPES::T_BINARY_PROTOCOL;
  }

  void sendRequest(
      uint8_t* buf,
      size_t len,
      int capability,
      HsStablePtr send_mvar,
      HsStablePtr recv_mvar,
      FinishedRequest* send_result,
      FinishedRequest* recv_result,
      apache::thrift::RpcOptions&& rpcOpts);

  void sendOnewayRequest(
      uint8_t* buf,
      size_t len,
      int capability,
      HsStablePtr send_mvar,
      FinishedRequest* send_result,
      apache::thrift::RpcOptions&& rpcOpts);

  InnerChannel* getInnerRequestChannel() {
    return client_.get();
  }

 private:
  enum RequestDirection {
    WITH_RESPONSE = 1,
    NO_RESPONSE = 2,
  };

  void sendRequestImpl(
      RequestDirection direction,
      apache::thrift::protocol::PROTOCOL_TYPES protocolId,
      CallbackPtr&& callback,
      std::unique_ptr<folly::IOBuf>&& message,
      apache::thrift::RpcOptions&& rpcOptions);

  template <typename F>
  void runOnClientEvbIfAvailable(F&& f) {
    if (auto evb = client_->get()->getEventBase()) {
      evb->add(std::forward<F>(f));
    } else {
      f();
    }
  }

  std::shared_ptr<InnerChannel> client_;
};

extern "C" {
std::shared_ptr<ChannelWrapper>* newWrapper(InnerChannel* channel) noexcept;

void deleteWrapper(std::shared_ptr<ChannelWrapper>* channel) noexcept;

void sendReq(
    std::shared_ptr<ChannelWrapper>* client,
    uint8_t* buf,
    size_t len,
    int capability,
    HsStablePtr send_mvar,
    HsStablePtr recv_mvar,
    FinishedRequest* send_result,
    FinishedRequest* recv_result,
    uint8_t* rpcOptionsPtr,
    size_t rpcOptionsLen) noexcept;

void sendOnewayReq(
    std::shared_ptr<ChannelWrapper>* client,
    uint8_t* buf,
    size_t len,
    int capability,
    HsStablePtr send_mvar,
    FinishedRequest* send_result,
    uint8_t* rpcOptionsPtr,
    size_t rpcOptionsLen) noexcept;

InnerChannel* getInnerRequestChannel(
    std::shared_ptr<ChannelWrapper>* client) noexcept;
}
