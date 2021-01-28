// Copyright (c) Facebook, Inc. and its affiliates.

#include "Util/AsanAlloc.h"

#include <malloc.h>

void* alignedAlloc(size_t alignment, size_t size) noexcept {
// GCC only supports ASAN aligned_alloc as of September 2014, which roughly
// corresponds to version 5 and above. This code can be deprecated once
// Facebook upgrade to GCC 5.x
#if __clang__ || __GNUC__ > 4
  return aligned_alloc(alignment, size);
#else
  return memalign(alignment, size);
#endif
}
