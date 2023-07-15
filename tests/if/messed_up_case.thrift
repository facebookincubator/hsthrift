/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef i64 (hs.type = "Int") intType

const string Str = "";

const string Str2 = Str;

enum numbers {
  one = 0,
  two = 1,
  three = 2,
}

struct foo {
  1: intType Field;
} (hs.prefix = "")

const numbers numberThing = one;
const numbers otherNumberThing = 1;

const foo fooConst = {"Field": 999};
