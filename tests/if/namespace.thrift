/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "if/namespace_included.thrift"

namespace hs Thrift.Test

struct X {
  1: i64 intField;
}

typedef namespace_included.X Y
typedef map<namespace_included.Y, list<namespace_included.Z>> Z
