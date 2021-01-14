// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

namespace {

template <typename T>
void destructImpl(T* p) noexcept {
  if (p != nullptr) {
    p->~T();
  }
}

template <typename T>
void deleteImpl(T* p) noexcept {
  delete p;
}

} // namespace

#ifndef HS_DEFINE_DESTRUCTIBLE
#define HS_DEFINE_DESTRUCTIBLE(Name, Type...)                     \
  extern "C" void destructible_destruct##Name(Type* p) noexcept { \
    destructImpl(p);                                              \
  }                                                               \
  extern "C" void destructible_delete##Name(Type* p) noexcept {   \
    deleteImpl(p);                                                \
  }
#endif
