/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

enum X {
  A = 0,
  B = 1,
  C = 2,
  D = 3,
  E = 4,
}

const X idConst = D;

const X intConst = 2;

enum Y {
}

enum Z {
  A = 2,
  B = 3,
}

const Z zconst = Z.A;
