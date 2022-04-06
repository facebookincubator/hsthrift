#pragma once

#include <memory>

namespace folly {

class RequestContext;

} // namespace folly

using RequestContextPtr = std::shared_ptr<folly::RequestContext>;
