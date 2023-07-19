/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef i64 X

struct Foo {
  1: optional i64 foo1;
  2: required bool foo2;
  3: Bar foo3;
  4: X foo4;
  5: list<i64> (hs.type = "Vector") foo5;
  6: list<i64> (hs.type = "VectorStorable") foo6;
}

struct Bar {
  -1: i32 bar1;
  1: string bar2;
}

enum Numbers {
  Zero = 0,
  One = 1,
  Three = 3,
  Four = 4,
  Five = 5,
  Seven = 7,
}

const i32 i32Const = 1;

const bool boolConst = 0;

const Foo fooConst = {
  "foo2": 1,
  "foo3": {"bar1": 99, "bar2": "hello world"},
  "foo4": 0,
  "foo5": [1, 2, 3],
  "foo6": [1, 2, 3],
};

const map<i32, list<i32>> mapConst = {0: [], 1: [1, 2, 3]};

typedef map<i64, i64> (hs.type = "HashMap") NewtypeMap (hs.newtype)

typedef string (hs.type = "String") HsString
