/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "thrift/annotation/haskell.thrift"

package "facebook.com/hs/thrift/tests/pseudoenum"

@haskell.PseudoEnum{value="thriftenum"}
enum PerfectEnum {
  W = 0,
  X = 1,
  Y = 2,
  Z = 3,
}
