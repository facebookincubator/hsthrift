/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

hs_include "if/instances.hs"

package "facebook.com/hs/thrift/tests/monoid"

struct X {
  1: list<i64> intList;
  2: list<string> stringList;
}
