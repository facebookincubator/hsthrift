// Copyright (c) Facebook, Inc. and its affiliates.

#include "cpp/logging.h"
#include <glog/logging.h>

int vlog_is_on(int level) noexcept {
  return VLOG_IS_ON(level);
}

#define LOG_EXT(severity, file, line) \
  google::LogMessage(file, line, ::google::GLOG_##severity).stream()

void glog_verbose(const char* file, int line, const char* msg) noexcept {
#ifdef GLOG_VERBOSE
  LOG_EXT(VERBOSE, file, line) << msg;
#else
  LOG_EXT(INFO, file, line) << msg;
#endif
}

void glog_info(const char* file, int line, const char* msg) noexcept {
  LOG_EXT(INFO, file, line) << msg;
}

void glog_warning(const char* file, int line, const char* msg) noexcept {
  LOG_EXT(WARNING, file, line) << msg;
}

void glog_error(const char* file, int line, const char* msg) noexcept {
  LOG_EXT(ERROR, file, line) << msg;
}

void glog_critical(const char* file, int line, const char* msg) noexcept {
  LOG_EXT(FATAL, file, line) << msg;
}
