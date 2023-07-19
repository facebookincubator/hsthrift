/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "if/bar.thrift"

struct Baz {
  1: bar.NewBar theBar;
  2: bar.HsStringVector theVector;
}
