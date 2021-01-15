// Copyright (c) Facebook, Inc. and its affiliates.

# @nolint

include "test/if/b.thrift"

typedef i64 T

struct A {
  1: T a = a,
  2: b.B b = b,
  3: bool c = b.bool_value,
  4: list<list<i32>> d,
  5: map<i32, string> e,
  6: b.Number f = b.Two,
  7: optional string g,
  8: required string h,
}

union U {
  1: byte x,
  2: list<string> y,
  3: set<i64> z,
}

exception X {
  1: string reason,
}

const T a = b.i64_value

const U u = { "y": [b.string_value] }

const b.B b = {
  "a": b.i16_value,
  "b": b.i32_value,
  "c": b.i64_value,
}

const b.B default_d = {}

const b.Number zero = b.Zero

service S {

  b.Number getNumber(1: i32 x);

  void doNothing() throws (1: X ex);

}

service ParentService {
}

service ChildService extends ParentService {
  i32 foo()
}
