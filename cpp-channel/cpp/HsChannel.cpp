// Copyright (c) Facebook, Inc. and its affiliates.

#include <cpp/HsChannel.h>
#include <if/gen-cpp2/RpcOptions_types.h>
#include <thrift/lib/cpp2/protocol/Serializer.h>

using namespace thrift::protocol;
using namespace apache::thrift::concurrency;

std::shared_ptr<ChannelWrapper>* newWrapper(InnerChannel* channel) noexcept {
  return new std::shared_ptr<ChannelWrapper>(
      std::make_shared<ChannelWrapper>(std::move(*channel)));
}

void deleteWrapper(std::shared_ptr<ChannelWrapper>* channel) noexcept {
  delete channel;
}

void ChannelWrapper::sendRequest(
    uint8_t* buf,
    size_t len,
    int capability,
    HsStablePtr send_mvar,
    HsStablePtr recv_mvar,
    FinishedRequest* send_result,
    FinishedRequest* recv_result,
    apache::thrift::RpcOptions&& rpcOpts) {
  auto msg = folly::IOBuf::wrapBuffer(buf, len);
  using CallbackPtr = std::unique_ptr<
      HsCallback,
      apache::thrift::RequestClientCallback::RequestClientCallbackDeleter>;
  auto cob = CallbackPtr(new HsCallback(
      client_, capability, send_mvar, recv_mvar, send_result, recv_result));

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
      cob->setMethodName(envelopeAndRequest->first.methodName);
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

void ChannelWrapper::sendOnewayRequest(
    uint8_t* buf,
    size_t len,
    int capability,
    HsStablePtr send_mvar,
    FinishedRequest* send_result,
    apache::thrift::RpcOptions&& rpcOpts) {
  auto msg = folly::IOBuf::wrapBuffer(buf, len);
  auto cob = apache::thrift::RequestClientCallback::Ptr(new HsCallback(
      client_, capability, send_mvar, nullptr, send_result, nullptr));

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

static_assert((int)Priority::HighImportant == (int)HIGH_IMPORTANT);
static_assert((int)Priority::High == (int)HIGH);
static_assert((int)Priority::Important == (int)IMPORTANT);
static_assert((int)Priority::NormalPriority == (int)NORMAL);
static_assert((int)Priority::BestEffort == (int)BEST_EFFORT);

apache::thrift::RpcOptions getRpcOptions(
    uint8_t* rpcOptionsPtr,
    size_t rpcOptionsLen) noexcept {
  apache::thrift::RpcOptions rpcOpts;
  auto tRpcOpts = apache::thrift::BinarySerializer::deserialize<
      thrift::protocol::RpcOptions>(
      folly::ByteRange(rpcOptionsPtr, rpcOptionsLen));
  rpcOpts.setTimeout(std::chrono::milliseconds(*tRpcOpts.timeout_ref()));
  auto priority = tRpcOpts.get_priority() == nullptr
      ? apache::thrift::RpcOptions::PRIORITY::NORMAL
      : static_cast<apache::thrift::RpcOptions::PRIORITY>(
            tRpcOpts.priority_ref().value_unchecked());
  rpcOpts.setPriority(priority);
  rpcOpts.setChunkTimeout(
      std::chrono::milliseconds(*tRpcOpts.chunkTimeout_ref()));
  rpcOpts.setQueueTimeout(
      std::chrono::milliseconds(*tRpcOpts.queueTimeout_ref()));
  if (tRpcOpts.get_headers() != nullptr) {
    for (auto const& header : tRpcOpts.headers_ref().value_unchecked()) {
      rpcOpts.setWriteHeader(header.first, header.second);
    }
  }
  return rpcOpts;
}

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
    size_t rpcOptionsLen) noexcept {
  apache::thrift::RpcOptions rpcOpts =
      getRpcOptions(rpcOptionsPtr, rpcOptionsLen);
  (*client)->sendRequest(
      buf,
      len,
      capability,
      send_mvar,
      recv_mvar,
      send_result,
      recv_result,
      std::move(rpcOpts));
}

void sendOnewayReq(
    std::shared_ptr<ChannelWrapper>* client,
    uint8_t* buf,
    size_t len,
    int capability,
    HsStablePtr send_mvar,
    FinishedRequest* send_result,
    uint8_t* rpcOptionsPtr,
    size_t rpcOptionsLen) noexcept {
  apache::thrift::RpcOptions rpcOpts =
      getRpcOptions(rpcOptionsPtr, rpcOptionsLen);
  (*client)->sendOnewayRequest(
      buf, len, capability, send_mvar, send_result, std::move(rpcOpts));
}

InnerChannel* getInnerRequestChannel(
    std::shared_ptr<ChannelWrapper>* client) noexcept {
  return (*client)->getInnerRequestChannel();
}
