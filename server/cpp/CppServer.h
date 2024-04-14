/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <unordered_set>

#include <thrift/lib/cpp2/async/AsyncProcessor.h>

#include "cpp/HaskellProcessor.h"

namespace apache {
namespace thrift {

// Given a callback and map of metadata, build a factory
using TFactory =
    AsyncProcessorFactory* (*)(TCallback,
                               AsyncProcessorFactory::MethodMetadataMap&);
} // namespace thrift
} // namespace apache
