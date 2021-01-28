// Copyright (c) Facebook, Inc. and its affiliates.

hs_include "if/hs_test_instances.hs"

typedef i64 X (hs.newtype, rust.newtype)
typedef X Y
typedef Y Z (hs.newtype, rust.newtype)

struct Foo {
  5: i32 bar,
  1: i32 baz,
}

union tUnion {
  1: string StringOption
  2: i64 I64Option
  3: Foo FooOption
}

struct TestStruct {
  1: required bool f_bool,
  2: byte f_byte,
  3: double f_double,
  4: i16 f_i16 = 5,
  5: i32 f_i32,
  6: i64 f_i64,
  7: float f_float,
  8: list<i16> f_list,
  9: map<i16,i32> f_map = {1:2},
  10: string f_text,
  11: set<byte> f_set,
  12: optional i32 o_i32,
  99: Foo foo = {"bar":1,"baz":2},
  13: map<Number, i64> (hs.type = "HashMap") f_hash_map,
  14: Z f_newtype,
  15: tUnion f_union,
  16: string (hs.type = "String") f_string,
  17: binary f_binary,
  18: optional X f_optional_newtype,
  19: map<i32,bool> bool_map,
  20: list<bool> bool_list,
  21: list<i64> (hs.type = "Vector") i64_vec,
  22: list<i64> (hs.type = "VectorStorable") i64_svec,
  23: map<binary, i64> binary_key,
  24: string (hs.type = "ByteString") f_bytestring,
}

enum Number {
  One = 1,
  Two = 2,
  Three = 3,
}

enum Perfect {
  A = 0,
  B = 1,
  C = 2,
}

enum Void {}
