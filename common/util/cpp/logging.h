// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

int vlog_is_on(int level) noexcept;

void glog_verbose(const char* file, int line, const char* msg) noexcept;
void glog_info(const char* file, int line, const char* msg) noexcept;
void glog_warning(const char* file, int line, const char* msg) noexcept;
void glog_error(const char* file, int line, const char* msg) noexcept;
void glog_critical(const char* file, int line, const char* msg) noexcept;
