/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "thrift/annotation/thrift.thrift"

struct Z {
  1: string name;
}

const Z z = {"name": "Z"};

service MyService {
  i64 testFunc(1: i64 arg1, 2: Z arg2 = z);

  void foo() throws (1: Ex ex) (hs.prefix = "x_");
}

service X {
  @thrift.Priority{level = thrift.RpcPriority.HIGH}
  i32 testFunc();
}

service Y extends X {
}

struct TestFunc {}

exception Ex {}

service Q {
  oneway void testFunc1();
  readonly i32 testFunc2();
}
