/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cpp/HaskellProcessor.h"
#include <glog/logging.h>

#include <thrift/lib/cpp2/util/LegacyRequestExpiryGuard.h>

namespace apache {
namespace thrift {

namespace {
const std::string kEx = "ex";
const std::string kUex = "uex";
const std::string kUexw = "uexw";

// The inbound Thrift `client_id` of the request being handled on this thread,
// or nullptr when the caller sent none. Only set for the duration of the
// synchronous callback into Haskell, so a Haskell handler must read it before
// handing work to another thread.
thread_local const std::string* tlInboundClientId = nullptr;

// Publishes `clientId` as this thread's inbound client_id, restoring whatever
// was there before on scope exit.
class InboundClientIdGuard {
 public:
  explicit InboundClientIdGuard(const std::string* clientId)
      : previous_(tlInboundClientId) {
    tlInboundClientId = clientId;
  }

  ~InboundClientIdGuard() {
    tlInboundClientId = previous_;
  }

  InboundClientIdGuard(const InboundClientIdGuard&) = delete;
  InboundClientIdGuard& operator=(const InboundClientIdGuard&) = delete;

 private:
  const std::string* previous_;
};
} // namespace

HaskellAsyncProcessor::HaskellAsyncProcessor(
    TCallback callback,
    AsyncProcessorFactory::MethodMetadataMap& metadataMap)
    : callback_(callback), metadataMap_(metadataMap) {}

void HaskellAsyncProcessor::run(
    apache::thrift::ResponseChannelRequest::UniquePtr req,
    apache::thrift::LegacySerializedRequest&& legacySerializedRequest,
    apache::thrift::Cpp2RequestContext* context,
    folly::EventBase* eb,
    TCallback cb,
    bool oneway,
    bool fromExecuteRequest) {
  // Mark the request as processing, so that it won't be failed
  // with a queue timeout.
  if (!fromExecuteRequest && req && !req->getShouldStartProcessing()) {
    // Queue timeout must have occurred. Clean up request and return.
    HandlerCallbackBase::releaseRequest(std::move(req), eb);
    return;
  }

  // EventTask only calles us if oneway || req->isActive()
  // Receive all the bytes and create a single buffer
  folly::ByteRange input_range = legacySerializedRequest.buffer->coalesce();
  auto input_data = input_range.data();
  auto input_len = input_range.size();

  // Send the bytes to Haskell and get the return bytes back. The inbound
  // client_id is published for the duration of this call: hsthrift does not
  // surface transport headers to Haskell handlers, and this is the one point
  // where we hold both the request context and the handler's thread.
  TResponse response;
  {
    InboundClientIdGuard clientIdGuard(context->clientId());
    (*cb)(
        context->getHeader()->getProtocolId(),
        input_data,
        input_len,
        &response);
  }
  std::unique_ptr<uint8_t[], decltype(free)*> output_str(response.data, free);

  for (auto& hdr : response.headers) {
    context->getHeader()->setHeader(hdr.first, hdr.second);
  }

  if (!oneway && req && req->isActive()) {
    apache::thrift::MessageType mtype;
    apache::thrift::ResponsePayload payload;
    try {
      // Take ownership of the output bytes into an IOBuf
      auto outbuf =
          folly::IOBuf::takeOwnership(std::move(output_str), response.len);

      // Send the output bytes along
      apache::thrift::LegacySerializedResponse legacySerializedResponse{
          std::move(outbuf)};
      std::tie(mtype, payload) = std::move(legacySerializedResponse)
                                     .extractPayload(
                                         req->includeEnvelope(),
                                         context->getHeader()->getProtocolId(),
                                         context->getProtoSeqId());
      payload.transform(context->getHeader()->getWriteTransforms());
    } catch (const std::exception& e) {
      if (!oneway) {
        const auto s = e.what();
        eb->runInEventBaseThread([req = std::move(req), s]() mutable {
          req->sendErrorWrapped(
              folly::make_exception_wrapper<TApplicationException>(
                  TApplicationException::TApplicationExceptionType::
                      INTERNAL_ERROR,
                  folly::to<std::string>(
                      "Failed to read response from Haskell: ", s)),
              "haskell");
        });
      }
      return;
    }
    eb->runInEventBaseThread([mtype = mtype,
                              req = std::move(req),
                              payload = std::move(payload)]() mutable {
      if (mtype == apache::thrift::MessageType::T_EXCEPTION) {
        req->sendException(std::move(payload));
      } else {
        req->sendReply(std::move(payload));
      }
    });
  }

  // either is inactive or oneway method, simply delete req in eb thread
  if (req) {
    eb->runInEventBaseThread([req = std::move(req)] {});
  }
}

bool isOneway(const AsyncProcessorFactory::MethodMetadata& meta) {
  if (auto rpc = meta.rpcKind) {
    return rpc == apache::thrift::RpcKind::SINGLE_REQUEST_NO_RESPONSE;
  }
  return false;
}

void HaskellAsyncProcessor::executeRequest(
    ServerRequest&& request,
    const AsyncProcessorFactory::MethodMetadata& meta) {
  using ServerRequestHelper = apache::thrift::detail::ServerRequestHelper;

  auto context = request.requestContext();
  auto protType = ServerRequestHelper::protocol(request);
  auto req = ServerRequestHelper::request(std::move(request));
  auto serializedCompressedRequest =
      ServerRequestHelper::compressedRequest(std::move(request));
  auto serializedRequest = std::move(serializedCompressedRequest).uncompress();
  auto* eb = apache::thrift::detail::ServerRequestHelper::eventBase(request);

  bool oneway = isOneway(meta);

  auto legacySerializedRequest = apache::thrift::LegacySerializedRequest(
      protType,
      context->getProtoSeqId(),
      context->getMethodName(),
      std::move(serializedRequest));

  run(std::move(req),
      std::move(legacySerializedRequest),
      context,
      eb,
      callback_,
      oneway,
      true /* fromExecuteRequest  */);
}

void HaskellAsyncProcessor::processSerializedCompressedRequestWithMetadata(
    apache::thrift::ResponseChannelRequest::UniquePtr req,
    apache::thrift::SerializedCompressedRequest&& serializedCompressedRequest,
    const apache::thrift::AsyncProcessorFactory::MethodMetadata& meta,
    apache::thrift::protocol::PROTOCOL_TYPES protType,
    apache::thrift::Cpp2RequestContext* context,
    folly::EventBase* eb,
    apache::thrift::concurrency::ThreadManager* tm) {
  // General note: we only want communicate (via req->sendReply or
  // req->sendErrorWrapped) and destroy req on eb's thread. We assume that we
  // are on eb's thread now but the EventTask that we create further below won't
  // necessarily be.

  auto serializedRequest = std::move(serializedCompressedRequest).uncompress();

  bool oneway = isOneway(meta);
  // Immediately give reply to one-way calls
  if (oneway && !req->isOneway()) {
    req->sendReply(ResponsePayload{});
  }

  // Adds a request handler to the thrift queue.
  auto task = [this,
               context,
               oneway,
               eb,
               cb = callback_,
               legacySerializedRequest =
                   apache::thrift::LegacySerializedRequest(
                       protType,
                       context->getProtoSeqId(),
                       context->getMethodName(),
                       std::move(serializedRequest)),
               req = std::move(req)]() mutable {
    run(std::move(req),
        std::move(legacySerializedRequest),
        context,
        eb,
        cb,
        oneway,
        false /* fromExecuteRequest  */);
  };

  const auto priorityFromHeaders = context->getCallPriority();
  const bool hasPriorityOverride =
      priorityFromHeaders != concurrency::PRIORITY::N_PRIORITIES;
  const auto pri = hasPriorityOverride ? priorityFromHeaders
      : meta.priority.has_value()      ? meta.priority.value()
                                       : apache::thrift::concurrency::NORMAL;

  DVLOG(5) << "Request priority: " << pri << " " << context->getMethodName();

  const auto source =
      apache::thrift::concurrency::ThreadManager::Source::UPSTREAM;
  // the ThreadManager can prioritise upstream jobs differently
  // from internal jobs. This also affects certain metrics, such
  // as thrift.queued_requests which only counts upstream jobs on
  // the queue.
  auto ka = tm->getKeepAlive(pri, source);
  ka->add(std::move(task));
}

extern "C" {

// Read the inbound Thrift `client_id` of the request currently being handled on
// this thread, for Thrift.Server.ClientId. Sets *len to its length and returns
// a pointer into memory owned by the request, so the caller must copy it before
// returning from the handler. Returns nullptr with *len 0 when the caller sent
// no client_id, or when called off a request thread.
const char* c_hsthrift_inbound_client_id(size_t* len) {
  if (tlInboundClientId == nullptr) {
    *len = 0;
    return nullptr;
  }
  *len = tlInboundClientId->size();
  return tlInboundClientId->data();
}

} // extern "C"

} // namespace thrift
} // namespace apache

// TODO move inside the namespace and resolve the C++ symbol from Haskell
extern "C" {
void addHeaderToResponse(
    apache::thrift::TResponse* response,
    const char* name_str,
    size_t name_len,
    const char* value_str,
    size_t value_len) {
  response->headers.emplace_back(
      std::string(name_str, name_len), std::string(value_str, value_len));
}
}
