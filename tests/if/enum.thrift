/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "thrift/annotation/haskell.thrift"

package "facebook.com/hs/thrift/tests/enum"

enum UnsortedEnum {
  G = 7,
  A = 1,
  D = 4,
  E = 5,
  C = 3,
  B = 2,
}

@haskell.NoUnknown
enum EnumWithNounknown {
  U = 0,
  V = 1,
}

enum PerfectEnum {
  W = 0,
  X = 1,
  Y = 2,
  Z = 3,
}
