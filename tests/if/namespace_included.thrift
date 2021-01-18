// Copyright (c) Facebook, Inc. and its affiliates.

namespace hs Thrift.Test.Internal

struct X {
  1: i64 intField
}

enum Y {
  Y1 = 0,
}

union Z {
  1: X x
  2: Y y
}
