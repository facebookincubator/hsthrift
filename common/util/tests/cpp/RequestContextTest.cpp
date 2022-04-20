#include <folly/io/async/Request.h>

#include "cpp/RequestContext.h"

namespace {

using TestRequestData = folly::ImmutableRequestData<int64_t>;
const folly::RequestToken kRequestToken("hs_request_context_test");

} // namespace

extern "C" {

int64_t hs_request_context_getTestValue(RequestContextPtr* p) noexcept {
  if (*p) {
    auto requestData =
        dynamic_cast<TestRequestData*>((*p)->getContextData(kRequestToken));
    return requestData != nullptr ? requestData->value() : 0;
  } else {
    return 0;
  }
}

void hs_request_context_setTestValue(
    RequestContextPtr* p,
    int64_t value) noexcept {
  (*p)->overwriteContextData(
      kRequestToken, std::make_unique<TestRequestData>(value));
}

} // extern "C"
