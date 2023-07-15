/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct X1 {
  1: i32 x;
}

struct X2 {
  1: string x;
}

struct Y1 {
  1: i64 x;
  2: string y;
}

struct Y2 {
  1: i64 x;
}

union U1 {
  1: i64 x;
  2: string y;
}

union U2 {
  1: i64 x;
}

struct L1 {
  1: list<U1> l;
}

struct L2 {
  1: list<U2> l;
}
