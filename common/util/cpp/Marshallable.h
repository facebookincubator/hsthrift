/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once
#include "cpp/Constructible.h"
#include "cpp/Destructible.h"

#ifndef HS_DEFINE_MARSHALLABLE
#define HS_DEFINE_MARSHALLABLE(Name, Type...) \
  HS_DEFINE_DEFAULT_CONSTRUCTIBLE(Name, Type) \
  HS_DEFINE_DESTRUCTIBLE(Name, Type)
#endif
