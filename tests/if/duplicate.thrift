/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

package "facebook.com/hs/thrift/tests/duplicate"

struct X {
  1: string name;
  2: i64 payload;
}

struct Y {
  1: string name;
  2: bool payload;
}
