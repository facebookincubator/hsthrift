// Copyright (c) Facebook, Inc. and its affiliates.

include "if/E.thrift"

# INCLUSION HIERARCHY
#
#        C
#        |
#        E

struct C {
  1: E.E eThing
}
