// Copyright (c) Facebook, Inc. and its affiliates.

# @nolint

# field/type are entirely ignored due to hs.hidden annotation
struct Foo {
  1: some_type bar (hs.hidden);
  2: some_type bar (hs.lazy, hs.hidden);
  3: i64 bar (hs.hidden);
}

service _Service {
  void doNothing(1: Foo f);
}
