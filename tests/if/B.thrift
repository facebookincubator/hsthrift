// Copyright (c) Facebook, Inc. and its affiliates.

include "if/D.thrift"
include "if/E.thrift"

# INCLUSION HIERARCHY
#
#        B
#      /   \
#     D     E

struct B {
  1: D.D dThing
  2: E.E eThing
  3: B2 b2Thing
}

enum B2 {
  X = 0,
  Y = 1,
}
