/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "thrift/annotation/haskell.thrift"

package "facebook.com/hs/thrift/tests/scoped_enums"

enum X {
  A = 1,
}

enum Y {
  A = -1,
}

@haskell.Prefix{name="a_"}
enum Z {
  A = 9,
}

const X x = X.A;
const X x2 = A;

const Y y = Y.A;

const Z z = Z.A;

@haskell.PseudoEnum
enum A {
  A = 1,
}

const A pseudo = A.A;

@haskell.PseudoEnum
@haskell.Prefix{name="enum_"}
enum B {
  B = 2,
}

const B prefix = B.B;
