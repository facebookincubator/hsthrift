#include "cpp/RequestContext.h"

#include <folly/io/async/Request.h>

#include "cpp/Destructible.h"

HS_DEFINE_DESTRUCTIBLE(RequestContextPtr, RequestContextPtr);

extern "C" {

RequestContextPtr* hs_request_context_saveContext() noexcept {
  auto rc = folly::RequestContext::saveContext();
  if (!rc) {
    rc = std::make_shared<folly::RequestContext>(0);
  }
  return new RequestContextPtr(std::move(rc));
}

void hs_request_context_setContext(RequestContextPtr* ptr) noexcept {
  folly::RequestContext::setContext(*ptr);
}

RequestContextPtr* hs_request_context_createShallowCopy(
    RequestContextPtr* ptr) noexcept {
  return new RequestContextPtr(folly::RequestContext::copyAsChild(**ptr));
}

} // extern "C"
