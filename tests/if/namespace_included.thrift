/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace hs Thrift.Test.Internal

struct X {
  1: i64 intField;
}

enum Y {
  Y1 = 0,
}

union Z {
  1: X x;
  2: Y y;
}
