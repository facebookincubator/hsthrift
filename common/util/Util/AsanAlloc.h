#pragma once

#include <stdlib.h>

extern "C" {

void* alignedAlloc(size_t alignment, size_t size) noexcept;
}
