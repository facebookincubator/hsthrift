/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "thrift/annotation/haskell.thrift"

package "facebook.com/hs/thrift/tests/hs_prefix"

@haskell.NoUnknown
enum E {
  A = 0,
  B = 1,
}

@haskell.Prefix{name="PE_"}
enum PrefixedE {
  A = 0,
  B = 1,
}

struct S {
  1: i64 A;
  2: E B = E.B;
}

struct PrefixedS {
  1: i64 A;
  3: PrefixedE B = PrefixedE.B;
} (hs.prefix = "ps_")

union U {
  1: E A;
  2: S B;
}

@haskell.Prefix{name="PU_"}
union PrefixedU {
  1: PrefixedE A;
  2: PrefixedS B;
}
