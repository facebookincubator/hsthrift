// Copyright 2008-present Facebook. All Rights Reserved.

#include <memory>

#include <glog/logging.h>

#include <folly/executors/IOThreadPoolExecutor.h>

extern "C" folly::IOThreadPoolExecutor* newExecutor() noexcept {
  return new folly::IOThreadPoolExecutor(
      sysconf(_SC_NPROCESSORS_ONLN),
      std::make_shared<folly::NamedThreadFactory>("HaskellIOThreadPool"));
}

extern "C" void destroyExecutor(folly::IOThreadPoolExecutor* ex) noexcept {
  DCHECK_NOTNULL(ex)->join();
  delete ex;
}

extern "C" folly::EventBase* getExecutorEventBase(
    folly::IOThreadPoolExecutor* io) noexcept {
  return DCHECK_NOTNULL(io)->getEventBase();
}
