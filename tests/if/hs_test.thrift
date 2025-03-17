/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

hs_include "if/hs_test_instances.hs"

typedef i64 X (hs.newtype)
typedef X Y
typedef Y Z (hs.newtype)

struct Foo {
  5: i32 bar;
  1: i32 baz;
}

union tUnion {
  1: string StringOption;
  2: i64 I64Option;
  3: Foo FooOption;
}

struct TestStruct {
  1: required bool f_bool;
  2: byte f_byte;
  3: double f_double;
  4: i16 f_i16 = 5;
  5: i32 f_i32;
  6: i64 f_i64;
  7: float f_float;
  8: list<i16> f_list;
  9: map<i16, i32> f_map = {1: 2};
  10: string f_text;
  11: set<byte> f_set;
  12: optional i32 o_i32;
  99: Foo foo = {"bar": 1, "baz": 2};
  13: map_Number_i64_1522 f_hash_map;
  14: Z f_newtype;
  15: tUnion f_union;
  16: string_5858 f_string;
  17: binary f_binary;
  18: optional X f_optional_newtype;
  19: map<i32, bool> bool_map;
  20: list<bool> bool_list;
  21: list_i64_7708 i64_vec;
  22: list_i64_1894 i64_svec;
  23: map<binary, i64> binary_key;
  24: string_1484 f_bytestring;
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

enum Void {
}

// The following were automatically generated and may benefit from renaming.
typedef list<i64> (hs.type = "VectorStorable") list_i64_1894
typedef list<i64> (hs.type = "Vector") list_i64_7708
typedef map<Number, i64> (hs.type = "HashMap") map_Number_i64_1522
typedef string (hs.type = "ByteString") string_1484
typedef string (hs.type = "String") string_5858
