// Copyright (c) Facebook, Inc. and its affiliates.

#include <folly/executors/GlobalExecutor.h>

extern "C" folly::Executor::KeepAlive<>*
common_hs_getGlobalCPUExecutor() noexcept {
  return new folly::Executor::KeepAlive<>(folly::getGlobalCPUExecutor());
}

extern "C" void common_hs_releaseGlobalCPUExecutor(
    folly::Executor::KeepAlive<>* e) noexcept {
  delete e;
}

extern "C" folly::Executor* common_hs_getExecutorFromKeepAlive(
    folly::Executor::KeepAlive<>* k) noexcept {
  return k->get();
}
