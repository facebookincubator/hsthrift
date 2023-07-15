/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "test/if/math.thrift"

service Echoer extends math.Calculator {
  string echo(1: string input);
}
