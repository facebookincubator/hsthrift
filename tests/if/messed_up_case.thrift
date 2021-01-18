// Copyright (c) Facebook, Inc. and its affiliates.

typedef i64 (hs.type = "Int") intType

const string Str = ""

const string Str2 = Str

enum numbers {
  one = 0,
  two = 1,
  three = 2,
}

struct foo {
  1: intType Field
} (hs.prefix = "")

const numbers numberThing = one
const numbers otherNumberThing = 1

const foo fooConst = { "Field" : 999 }
