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
  auto cob = CallbackPtr(new HsCallback(
      client_, capability, send_mvar, recv_mvar, send_result, recv_result));
  sendRequestImpl(
      ChannelWrapper::RequestDirection::WITH_RESPONSE,
      getProtocolType(buf[0]),
      std::move(cob),
      std::move(msg),
      std::move(rpcOpts));
}

void ChannelWrapper::sendOnewayRequest(
    uint8_t* buf,
    size_t len,
    int capability,
    HsStablePtr send_mvar,
    FinishedRequest* send_result,
    apache::thrift::RpcOptions&& rpcOpts) {
  auto msg = folly::IOBuf::wrapBuffer(buf, len);
  auto cob = CallbackPtr(new HsCallback(
      client_, capability, send_mvar, nullptr, send_result, nullptr));
  sendRequestImpl(
      ChannelWrapper::RequestDirection::NO_RESPONSE,
      getProtocolType(buf[0]),
      std::move(cob),
      std::move(msg),
      std::move(rpcOpts));
}

void ChannelWrapper::sendRequestImpl(
    ChannelWrapper::RequestDirection requestDirection,
    apache::thrift::protocol::PROTOCOL_TYPES protocolId,
    CallbackPtr&& callback,
    std::unique_ptr<folly::IOBuf>&& message,
    apache::thrift::RpcOptions&& rpcOptions) {
  auto header = apache::thrift::transport::THeader(0);
  header.setProtocolId(protocolId);
  header.setHeaders(rpcOptions.releaseWriteHeaders());

  auto envelopeAndRequest =
      apache::thrift::EnvelopeUtil::stripRequestEnvelope(std::move(message));
  if (!envelopeAndRequest.has_value()) {
    callback.release()->onResponseError(
        folly::make_exception_wrapper<
            apache::thrift::transport::TTransportException>(
            apache::thrift::transport::TTransportException::CORRUPTED_DATA,
            "Unexpected problem stripping envelope"));
    return;
  }

  auto envelope = std::move(envelopeAndRequest->first);
  auto buf = std::move(envelopeAndRequest->second);
  callback->setMethodName(envelope.methodName);

  auto request = apache::thrift::SerializedRequest(std::move(buf));
  runOnClientEvbIfAvailable([client = client_,
                             requestDirection = requestDirection,
                             rpcOptions = std::move(rpcOptions),
                             request = std::move(request),
                             header = std::move(header),
                             envelope = std::move(envelope),
                             callback = std::move(callback)]() mutable {
    switch (requestDirection) {
      case ChannelWrapper::RequestDirection::WITH_RESPONSE:
        client->get()->sendRequestResponse(
            std::move(rpcOptions),
            envelope.methodName,
            std::move(request),
            std::make_shared<apache::thrift::transport::THeader>(
                std::move(header)),
            std::move(callback));
        break;
      case ChannelWrapper::RequestDirection::NO_RESPONSE:
        client->get()->sendRequestNoResponse(
            std::move(rpcOptions),
            envelope.methodName,
            std::move(request),
            std::make_shared<apache::thrift::transport::THeader>(
                std::move(header)),
            std::move(callback));
        break;
    }
  });
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
