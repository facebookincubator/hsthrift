/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace cpp2 "thrift.protocol"
namespace hs "Thrift.Protocol"

enum Priority {
  HighImportant = 0,
  High = 1,
  Important = 2,
  NormalPriority = 3,
  BestEffort = 4,
} (hs.prefix = "")

struct RpcOptions {
  1: i32 timeout;
  2: optional Priority priority;
  3: i32 chunkTimeout;
  4: i32 queueTimeout;
  5: optional map<string, string> headers;
} (hs.prefix = "rpc_")
