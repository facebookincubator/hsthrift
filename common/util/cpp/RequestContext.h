/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/io/async/Request.h>
#include <memory>
#include <optional>

// The address of folly::RequestContext should be constant and nonnull.
// The content of folly::RequestContext may be changed.
using RequestContextPtr = const std::shared_ptr<folly::RequestContext>;

namespace facebook::common::hs {

/**
 * In a foreign function call from Haskell to C++, the caller can pass an
 * explicit <tt>RequestContext</tt> (nonnull <tt>RequestContextPtr</tt>) or
 * <tt>Maybe RequestContext</tt> (nullable <tt>RequestContextPtr</tt>).
 * The C++ implementation should construct a \c RequestContextPtrScopeGuard
 * at the entry point to ensure the correct implicit \c folly::RequestContext
 * is always used in C++. There is usually no need to extend the lifetime of
 * this scope guard beyond the foreign function itself as long as all
 * async executions scheduled by it use async frameworks which are
 * RequestContext aware, e.g. folly futures and fibers.
 */
class RequestContextPtrScopeGuard {
 public:
  explicit RequestContextPtrScopeGuard(RequestContextPtr* rc) {
    if (rc != nullptr) {
      guard_.emplace(*rc);
    }
  }

  RequestContextPtrScopeGuard(const RequestContextPtrScopeGuard&) = delete;
  RequestContextPtrScopeGuard(RequestContextPtrScopeGuard&&) = delete;
  RequestContextPtrScopeGuard& operator=(const RequestContextPtrScopeGuard&) =
      delete;
  RequestContextPtrScopeGuard& operator=(RequestContextPtrScopeGuard&&) =
      delete;
  ~RequestContextPtrScopeGuard() = default;

 private:
  std::optional<folly::RequestContextScopeGuard> guard_;
};

} // namespace facebook::common::hs
