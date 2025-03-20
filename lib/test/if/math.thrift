/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "thrift/annotation/thrift.thrift"

exception DivideByZero {}

service Adder {
  i64 add(1: i64 x, 2: i64 y);
}

struct QuotRemResponse {
  1: i64 quot;
  2: i64 rem;
}

service Calculator extends Adder {
  double divide(1: double dividend, 2: double divisor) throws (
    1: DivideByZero divisionError,
  );

  QuotRemResponse quotRem(1: i64 dividend, 2: i64 divisor);

  oneway void put(1: i64 val);

  @thrift.DeprecatedUnvalidatedAnnotations{items = {"haxl.batched": "1"}}
  oneway void putMany(1: list<i64> val);

  i64 get();

  void unimplemented();
}
