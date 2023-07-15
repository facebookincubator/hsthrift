/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

enum X {
  A = 1,
}

enum Y {
  A = -1,
}

enum Z {
  A = 9,
} (hs.prefix = "a_")

const X x = X.A;
const X x2 = A;

const Y y = Y.A;

const Z z = Z.A;

enum A {
  A = 1,
} (hs.pseudoenum)

const A pseudo = A.A;

enum B {
  B = 2,
} (hs.pseudoenum, hs.prefix = 'enum_')

const B prefix = B.B;
const B prx2 = B;
