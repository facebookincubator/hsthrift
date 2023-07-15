/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "cpp/RequestContext.h"

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
