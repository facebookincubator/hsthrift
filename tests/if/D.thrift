// Copyright (c) Facebook, Inc. and its affiliates.

typedef i64 (hs.type = "Int") Int

const Int int_val = 123

struct D {
  1: Int dInt = int_val
}
