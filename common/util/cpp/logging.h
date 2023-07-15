/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

int vlog_is_on(int level) noexcept;

void glog_verbose(const char* file, int line, const char* msg) noexcept;
void glog_info(const char* file, int line, const char* msg) noexcept;
void glog_warning(const char* file, int line, const char* msg) noexcept;
void glog_error(const char* file, int line, const char* msg) noexcept;
void glog_fatal(const char* file, int line, const char* msg) noexcept;
