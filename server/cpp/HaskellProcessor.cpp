// Copyright (c) Facebook, Inc. and its affiliates.

#include "cpp/HaskellProcessor.h"

#include <thrift/lib/cpp2/util/LegacyRequestExpiryGuard.h>

namespace apache {
namespace thrift {

namespace {
const std::string kEx = "ex";
const std::string kUex = "uex";
const std::string kUexw = "uexw";
} // namespace

HaskellAsyncProcessor::HaskellAsyncProcessor(
    TCallback callback,
    const std::unordered_set<std::string>& oneways)
    : callback_(callback), oneways_(oneways) {}

void HaskellAsyncProcessor::run(
    apache::thrift::ResponseChannelRequest::UniquePtr req,
    apache::thrift::LegacySerializedRequest&& legacySerializedRequest,
    apache::thrift::Cpp2RequestContext* context,
    folly::EventBase* eb,
    TCallback cb,
    bool oneway) {
  // Mark the request as processing, so that it won't be failed
  // with a queue timeout.
  if (req && !req->getShouldStartProcessing()) {
    // Queue timeout must have occurred. Clean up request and return.
    HandlerCallbackBase::releaseRequest(std::move(req), eb);
    return;
  }

  // EventTask only calles us if oneway || req->isActive()
  // Receive all the bytes and create a single buffer
  folly::ByteRange input_range = legacySerializedRequest.buffer->coalesce();
  auto input_data = input_range.data();
  auto input_len = input_range.size();

  // Send the bytes to Haskell and get the return bytes back
  TResponse response;
  (*cb)(
      context->getHeader()->getProtocolId(), input_data, input_len, &response);
  std::unique_ptr<uint8_t[], decltype(free)*> output_str(response.data, free);
  SCOPE_EXIT {
    free(response.ex_name);
    free(response.ex_text);
  };
  if (response.ex_name) {
    auto header = context->getHeader();
    header->setHeader(
        kUex, std::string(response.ex_name, response.ex_name_len));
    if (response.ex_text) {
      header->setHeader(
          kUexw, std::string(response.ex_text, response.ex_text_len));
    }
    header->setHeader(
        kEx, response.client_error ? kAppClientErrorCode : kAppServerErrorCode);
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

void HaskellAsyncProcessor::executeRequest(
    ServerRequest&& request,
    const AsyncProcessorFactory::MethodMetadata& methodMetadata) {
  using ServerRequestHelper = apache::thrift::detail::ServerRequestHelper;

  auto context = request.requestContext();
  auto protType = ServerRequestHelper::protocol(request);
  auto req = ServerRequestHelper::request(std::move(request));
  auto serializedCompressedRequest =
      ServerRequestHelper::compressedRequest(std::move(request));
  auto serializedRequest = std::move(serializedCompressedRequest).uncompress();
  auto* eb = apache::thrift::detail::ServerRequestHelper::eventBase(request);

  bool oneway = oneways_.find(context->getMethodName()) != oneways_.end();

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
      oneway);
}

void HaskellAsyncProcessor::processSerializedCompressedRequestWithMetadata(
    apache::thrift::ResponseChannelRequest::UniquePtr req,
    apache::thrift::SerializedCompressedRequest&& serializedCompressedRequest,
    const apache::thrift::AsyncProcessorFactory::MethodMetadata&,
    apache::thrift::protocol::PROTOCOL_TYPES protType,
    apache::thrift::Cpp2RequestContext* context,
    folly::EventBase* eb,
    apache::thrift::concurrency::ThreadManager* tm) {
  // General note: we only want communicate (via req->sendReply or
  // req->sendErrorWrapped) and destroy req on eb's thread. We assume that we
  // are on eb's thread now but the EventTask that we create further below won't
  // necessarily be.

  auto serializedRequest = std::move(serializedCompressedRequest).uncompress();

  // Immediately give reply to one-way calls
  bool oneway = oneways_.find(context->getMethodName()) != oneways_.end();
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
        oneway);
  };

  const auto priorityFromHeaders = context->getCallPriority();
  const auto pri = (priorityFromHeaders != concurrency::PRIORITY::N_PRIORITIES)
      ? priorityFromHeaders
      : apache::thrift::concurrency::NORMAL;

  const auto source =
      apache::thrift::concurrency::ThreadManager::Source::UPSTREAM;
  // the ThreadManager can prioritise upstream jobs differently
  // from internal jobs. This also affects certain metrics, such
  // as thrift.queued_requests which only counts upstream jobs on
  // the queue.
  auto ka = tm->getKeepAlive(pri, source);
  ka->add(std::move(task));
}

void HaskellAsyncProcessor::processSerializedRequest(
    ResponseChannelRequest::UniquePtr req,
    apache::thrift::SerializedRequest&& serializedRequest,
    protocol::PROTOCOL_TYPES protType,
    Cpp2RequestContext* context,
    folly::EventBase* eb,
    concurrency::ThreadManager* tm) {
  processSerializedCompressedRequestWithMetadata(
      std::move(req),
      apache::thrift::SerializedCompressedRequest(std::move(serializedRequest)),
      apache::thrift::AsyncProcessorFactory::MethodMetadata(),
      protType,
      context,
      eb,
      tm);
}

} // namespace thrift
} // namespace apache
