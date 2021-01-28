// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include <unordered_set>

#include <thrift/lib/cpp2/async/AsyncProcessor.h>

#include "cpp/HaskellProcessor.h"

namespace apache {
namespace thrift {

// Given a callback and set of oneway functions, build a factory
using TFactory =
    AsyncProcessorFactory* (*)(TCallback, std::unordered_set<std::string>&);
} // namespace thrift
} // namespace apache
