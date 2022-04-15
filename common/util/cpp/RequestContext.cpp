#include "cpp/RequestContext.h"

#include <folly/io/async/Request.h>

#include "cpp/Destructible.h"

HS_DEFINE_DESTRUCTIBLE(RequestContextPtr, RequestContextPtr);

extern "C" {

RequestContextPtr* hs_request_context_saveContext() noexcept {
  return new RequestContextPtr(folly::RequestContext::saveContext());
}

void hs_request_context_setContext(RequestContextPtr* ptr) noexcept {
  folly::RequestContext::setContext(*ptr);
}

RequestContextPtr* hs_request_context_createShallowCopy(
    RequestContextPtr* ptr) noexcept {
  if (*ptr) {
    return new RequestContextPtr(folly::RequestContext::copyAsChild(**ptr));
  } else {
    return new RequestContextPtr(std::make_shared<folly::RequestContext>(0));
  }
}

} // extern "C"
