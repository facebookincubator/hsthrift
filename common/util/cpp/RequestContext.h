#pragma once

#include <memory>

namespace folly {

class RequestContext;

} // namespace folly

// The address of folly::RequestContext should be constant and nonnull.
// The content of folly::RequestContext may be changed.
using RequestContextPtr = const std::shared_ptr<folly::RequestContext>;
