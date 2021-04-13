// Copyright (c) Facebook, Inc. and its affiliates.

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
      int cap,
      HsStablePtr send_mvar,
      HsStablePtr recv_mvar,
      FinishedRequest* send_result,
      FinishedRequest* recv_result)
      : cap_(cap),
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

  void onRequestSent() noexcept override {
    send_result_->status = SEND_SUCCESS;
    hs_try_putmvar(cap_, send_mvar_);
    requestSent_ = true;
    if (!recv_result_) {
      delete this;
    }
  }

  void onResponseError(folly::exception_wrapper ew) noexcept override {
    auto ex = ew.what();
    size_t len = ex.length();
    auto buf = std::unique_ptr<uint8_t, decltype(free)*>{
        reinterpret_cast<uint8_t*>(malloc(len * sizeof(uint8_t))), free};
    std::memcpy(buf.get(), ex.data(), len);

    // If we have already enqueued a FinishedRequest for requestSent(), then
    // this needs to be be reported in the RECV round
    if (requestSent_) {
      recv_result_->status = RECV_ERROR;
      recv_result_->buffer = buf.release();
      recv_result_->len = len;
      hs_try_putmvar(cap_, recv_mvar_);
    } else {
      send_result_->status = SEND_ERROR;
      send_result_->buffer = buf.release();
      send_result_->len = len;
      hs_try_putmvar(cap_, send_mvar_);
    }
    delete this;
  }

  void onResponse(
      apache::thrift::ClientReceiveState&& state) noexcept override {
    if (state.isException()) {
      auto ex = state.exception().what();
      size_t len = ex.length();
      auto buf = std::unique_ptr<uint8_t, decltype(free)*>{
          reinterpret_cast<uint8_t*>(malloc(len * sizeof(uint8_t))), free};
      std::memcpy(buf.get(), ex.data(), len);

      recv_result_->status = RECV_ERROR;
      recv_result_->buffer = buf.release();
      recv_result_->len = len;
      hs_try_putmvar(cap_, recv_mvar_);
    } else {
      auto ioBuf = state.extractBuf();
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
    delete this;
  }

 private:
  int cap_;
  HsStablePtr send_mvar_;
  HsStablePtr recv_mvar_;
  FinishedRequest* send_result_;
  FinishedRequest* recv_result_;
  bool requestSent_ = false;
};

class ChannelWrapper {
 public:
  explicit ChannelWrapper(InnerChannel client)
      : client_(std::make_shared<InnerChannel>(std::move(client))){};

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
      apache::thrift::RpcOptions&& rpcOpts) {
    auto msg = folly::IOBuf::wrapBuffer(buf, len);
    auto cob = apache::thrift::RequestClientCallback::Ptr(new HsCallback(
        capability, send_mvar, recv_mvar, send_result, recv_result));

    auto sendRequestImpl = [protId = getProtocolType(buf[0]),
                            client = client_,
                            cob = std::move(cob),
                            msg = std::move(msg),
                            rpcOpts = std::move(rpcOpts)]() mutable {
      auto header = std::make_shared<apache::thrift::transport::THeader>(0);
      header->setProtocolId(protId);
      for (auto const& rpcHeader : rpcOpts.getWriteHeaders()) {
        header->setHeader(rpcHeader.first, rpcHeader.second);
      }

      if (auto envelopeAndRequest =
              apache::thrift::EnvelopeUtil::stripRequestEnvelope(
                  std::move(msg))) {
        client->get()->sendRequestResponse(
            rpcOpts,
            envelopeAndRequest->first.methodName,
            apache::thrift::SerializedRequest(
                std::move(envelopeAndRequest->second)),
            header,
            std::move(cob));
      } else {
        cob.release()->onResponseError(
            folly::make_exception_wrapper<
                apache::thrift::transport::TTransportException>(
                apache::thrift::transport::TTransportException::CORRUPTED_DATA,
                "Unexpected problem stripping envelope"));
      }
    };

    if (auto evb = client_->get()->getEventBase()) {
      evb->add(std::move(sendRequestImpl));
    } else {
      sendRequestImpl();
    }
  }

  void sendOnewayRequest(
      uint8_t* buf,
      size_t len,
      int capability,
      HsStablePtr send_mvar,
      FinishedRequest* send_result,
      apache::thrift::RpcOptions&& rpcOpts) {
    auto msg = folly::IOBuf::wrapBuffer(buf, len);
    auto cob = apache::thrift::RequestClientCallback::Ptr(
        new HsCallback(capability, send_mvar, nullptr, send_result, nullptr));

    auto sendOnewayRequestImpl = [protId = getProtocolType(buf[0]),
                                  client = client_,
                                  cob = std::move(cob),
                                  msg = std::move(msg),
                                  rpcOpts = std::move(rpcOpts)]() mutable {
      auto header = std::make_shared<apache::thrift::transport::THeader>(0);
      header->setProtocolId(protId);
      for (auto const& rpcHeader : rpcOpts.getWriteHeaders()) {
        header->setHeader(rpcHeader.first, rpcHeader.second);
      }

      if (auto envelopeAndRequest =
              apache::thrift::EnvelopeUtil::stripRequestEnvelope(
                  std::move(msg))) {
        client->get()->sendRequestNoResponse(
            rpcOpts,
            envelopeAndRequest->first.methodName,
            apache::thrift::SerializedRequest(
                std::move(envelopeAndRequest->second)),
            header,
            std::move(cob));
      } else {
        cob.release()->onResponseError(
            folly::make_exception_wrapper<
                apache::thrift::transport::TTransportException>(
                apache::thrift::transport::TTransportException::CORRUPTED_DATA,
                "Unexpected problem stripping envelope"));
      }
    };
    if (auto evb = client_->get()->getEventBase()) {
      evb->add(std::move(sendOnewayRequestImpl));
    } else {
      sendOnewayRequestImpl();
    }
  }

  InnerChannel* getInnerRequestChannel() {
    return client_.get();
  }

 private:
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
