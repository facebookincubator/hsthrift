// Copyright (c) Facebook, Inc. and its affiliates.

enum E {
  A = 0,
  B = 1,
} (hs.nounknown)

enum PrefixedE {
  A = 0,
  B = 1,
} (hs.prefix = "PE_")

struct S {
  1: i64 A
  2: E B = E.B
}

struct PrefixedS {
  1: i64 A
  3: PrefixedE B = PrefixedE.B
} (hs.prefix = "ps_")

union U {
  1: E A
  2: S B
}

union PrefixedU {
  1: PrefixedE A
  2: PrefixedS B
} (hs.prefix = "PU_")
