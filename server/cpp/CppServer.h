// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include <unordered_set>

#include <thrift/lib/cpp2/async/AsyncProcessor.h>

#include "cpp/HaskellProcessor.h"

namespace apache {
namespace thrift {

// Given a callback and map of metadata, build a factory
using TFactory =
    AsyncProcessorFactory* (*)(TCallback, AsyncProcessorFactory::MethodMetadataMap&);
} // namespace thrift
} // namespace apache
