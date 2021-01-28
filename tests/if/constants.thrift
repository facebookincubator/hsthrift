// Copyright (c) Facebook, Inc. and its affiliates.

const i32 i32Const = 99

const bool boolConst = 0

const float floatConst = 0.5

enum X {
  A = 0,
  B = 1,
  C = 2,
  D = 3,
  E = 4,
}

const X enumNum = 1

const X enumConst = D

const list<X> enumList = [ A, B, C, D ]

const map<i64, string> mapConst = { 0 : 'zero', 1 : 'one' }

struct Foo {
  1: optional i32 foo
  2: string bar = "X"
}

const Foo fooConst = { 'bar' : 'hello world' }
const Foo partial = {}

typedef i64 Id (hs.newtype)

const Id idConst = 12345

const list<Id> (hs.type = "Vector") idVect = [1, 2, 3, 4, 5]

const list<i64> (hs.type = "VectorStorable") i64VectS = [1, 2, 3, 4, 5]

const map<i64, string> (hs.type = "HashMap")
  hashmapConst = { 0 : 'zero', 1 : 'one' }

const string (hs.type = "String") strConst = "string"

const string (hs.type = "ByteString") byteStrConst = "string"

const i64 negative = -1

struct NagativeFields {
  1: required i64 u = negative
  2: optional i64 v = -1
  3: i64 w = -2
}

union NonEmpty {
  1: i64 ne
} (hs.nonempty)

const bool trueConst = true;
const bool falseConst = false;
