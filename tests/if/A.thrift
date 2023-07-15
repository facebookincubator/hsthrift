/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "if/B.thrift"
include "if/C.thrift"
include "if/D.thrift"
include "if/E.thrift"

# INCLUSION HIERARCHY
#
#        A
#      /   \
#     B     C
#   /   \ /
#  D     E

struct A {
  1: B.B bThing = {"dThing": {}, "eThing": {}, "b2Thing": B.B2.X};
  2: C.C cThing;
}

const D.D dstruct = {};
