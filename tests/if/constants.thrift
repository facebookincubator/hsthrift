/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "thrift/annotation/thrift.thrift"

const i32 i32Const = 99;

const bool boolConst = 0;

const float floatConst = 0.5;

enum X {
  A = 0,
  B = 1,
  C = 2,
  D = 3,
  E = 4,
}

const X enumNum = 1;

const X enumConst = D;

const list<X> enumList = [A, B, C, D];

const map<i64, string> mapConst = {0: 'zero', 1: 'one'};

struct Foo {
  1: optional i32 foo;
  2: string bar = "X";
}

const Foo fooConst = {'bar': 'hello world'};
const Foo partial = {};

typedef i64 Id (hs.newtype)

const Id idConst = 12345;

const list_Id_1134 idVect = [1, 2, 3, 4, 5];

const list_i64_5078 i64VectS = [1, 2, 3, 4, 5];

const map_i64_string_4289 hashmapConst = {0: 'zero', 1: 'one'};

const string_9126 strConst = "string";

const string_9425 byteStrConst = "string";

const i64 negative = -1;

struct NagativeFields {
  1: required i64 u = negative;
  @thrift.AllowUnsafeOptionalCustomDefaultValue
  2: optional i64 v = -1;
  3: i64 w = -2;
}

union NonEmpty {
  1: i64 ne;
} (hs.nonempty)

const bool trueConst = true;
const bool falseConst = false;

// The following were automatically generated and may benefit from renaming.
typedef list<Id> (hs.type = "Vector") list_Id_1134
typedef list<i64> (hs.type = "VectorStorable") list_i64_5078
typedef map<i64, string> (hs.type = "HashMap") map_i64_string_4289
typedef string (hs.type = "String") string_9126
typedef string (hs.type = "ByteString") string_9425
