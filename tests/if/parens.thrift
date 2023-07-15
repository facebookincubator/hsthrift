/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct X {
  # both Y and Int include <$> in their type parsers, so we need to insert parens
  # around these expressions
  1: map<Y, i64_4156> foo;
}

enum Y {
  A = 0,
  B = 1,
  C = 2,
}

// The following were automatically generated and may benefit from renaming.
typedef i64 (hs.type = "Int") i64_4156
