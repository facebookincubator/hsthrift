// Copyright (c) Facebook, Inc. and its affiliates.

#include <memory>

#include <glog/logging.h>

#include <folly/executors/IOThreadPoolExecutor.h>

extern "C" folly::IOThreadPoolExecutor*
common_hs_eventbase_newExecutor() noexcept {
  return new folly::IOThreadPoolExecutor(
      sysconf(_SC_NPROCESSORS_ONLN),
      std::make_shared<folly::NamedThreadFactory>("HaskellIOThreadPool"));
}

extern "C" void common_hs_eventbase_destroyExecutor(
    folly::IOThreadPoolExecutor* ex) noexcept {
  DCHECK_NOTNULL(ex)->join();
  delete ex;
}

extern "C" folly::EventBase* common_hs_eventbase_getIOExecutorEventBase(
    folly::IOThreadPoolExecutor* io) noexcept {
  return DCHECK_NOTNULL(io)->getEventBase();
}

extern "C" folly::Executor* common_hs_eventbase_castIOExecutorToExecutor(
    folly::IOThreadPoolExecutor* io) noexcept {
  return static_cast<folly::Executor*>(io);
}

extern "C" folly::Executor* common_hs_eventbase_castEventBaseToExecutor(
    folly::EventBase* io) noexcept {
  return static_cast<folly::Executor*>(io);
}
