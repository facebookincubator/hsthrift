/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef Foo Bar

struct Foo {
  1: i16 a;
}

const Bar BAR = Bar{a = 1};
