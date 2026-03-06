/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "if/E.thrift"
package "facebook.com/hs/thrift/tests/c"

# INCLUSION HIERARCHY
#
#        C
#        |
#        E

struct C {
  1: E.E eThing;
}
