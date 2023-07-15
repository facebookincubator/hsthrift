/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "if/D.thrift"
include "if/E.thrift"

# INCLUSION HIERARCHY
#
#        B
#      /   \
#     D     E

struct B {
  1: D.D dThing;
  2: E.E eThing;
  3: B2 b2Thing;
}

enum B2 {
  X = 0,
  Y = 1,
}
