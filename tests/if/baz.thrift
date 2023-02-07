// Copyright (c) Facebook, Inc. and its affiliates.

include "if/bar.thrift"

struct Baz {
  1: bar.NewBar theBar;
}
