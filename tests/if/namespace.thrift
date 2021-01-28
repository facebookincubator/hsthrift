// Copyright (c) Facebook, Inc. and its affiliates.

include "if/namespace_included.thrift"

namespace hs Thrift.Test

struct X {
  1: i64 intField
}

typedef namespace_included.X Y
typedef map<namespace_included.Y, list<namespace_included.Z>> Z
