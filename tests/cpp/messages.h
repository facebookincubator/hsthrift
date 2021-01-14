// Copyright (c) Facebook, Inc. and its affiliates.

#include <cstdint>

#include <sys/types.h>

extern "C" {
size_t echoJSON(const char* data, size_t len, uint8_t** buf) noexcept;
size_t echoBinary(const char* data, size_t len, uint8_t** buf) noexcept;
size_t echoCompact(const char* data, size_t len, uint8_t** buf) noexcept;
size_t echoBinaryCompact(const char* data, size_t len, uint8_t** buf) noexcept;
size_t echoCompactBinary(const char* data, size_t len, uint8_t** buf) noexcept;
}
