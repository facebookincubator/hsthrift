// Copyright (c) Facebook, Inc. and its affiliates.

# @nolint

namespace hs Namespace
typedef i64 _Type

exception _Exception {
  1: string reason;
}

const _Type a = 100;

struct _Struct {
  1: _Type a = a;
  2: list<list<_Type>> b;
}

union _Union {
  1: byte x;
  2: list<string> y;
  3: set<i64> z;
}

const _Union u = {"y": ["test"]};

service _Service {
  _Type getNumber(1: i32 x);

  void doNothing() throws (1: _Exception ex);
}
