#include <folly/io/async/Request.h>

#include "cpp/Destructible.h"

using RequestContextPtr = std::shared_ptr<folly::RequestContext>;

HS_DEFINE_DESTRUCTIBLE(RequestContextPtr, RequestContextPtr);

extern "C" {

RequestContextPtr* hs_request_context_saveContext() noexcept {
  return new RequestContextPtr(folly::RequestContext::saveContext());
}

void hs_request_context_setContext(RequestContextPtr* ptr) noexcept {
  folly::RequestContext::setContext(*ptr);
}

} // extern "C"
