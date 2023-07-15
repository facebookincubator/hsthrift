/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef i64 (hs.type = "Int") Int

const Int int_val = 123;

struct D {
  1: Int dInt = int_val;
}
