// Copyright (c) Facebook, Inc. and its affiliates.

enum UnsortedEnum {
  G = 7
  A = 1
  D = 4
  E = 5
  C = 3
  B = 2
}

enum EnumWithNounknown {
  U = 0,
  V = 1,
} (hs.nounknown)

enum PerfectEnum {
  W = 0,
  X = 1,
  Y = 2,
  Z = 3,
}
