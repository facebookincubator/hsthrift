// Copyright (c) Facebook, Inc. and its affiliates.

struct X {
  # both Y and Int include <$> in their type parsers, so we need to insert parens
  # around these expressions
  1: map<Y, i64 (hs.type = "Int")> foo
}

enum Y {
  A = 0,
  B = 1,
  C = 2,
}
