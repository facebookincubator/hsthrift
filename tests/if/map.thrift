/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef map<byte, string> Map

typedef string KeyType (hs.newtype)

struct X {
  1: map<i32, string> intMap;
  2: Map otherMap;
  3: optional map<i64, i64> optMap;
  4: map<i64, map<i64, i64>> nestedMap;
  5: list<map<i32, i32>> listMap;
  6: map_Y_string_2031 structMap;
  7: map<KeyType, i64> newtypeMap;
} (hs.prefix = "")

struct Y {
  1: i64 y;
}

// The following were automatically generated and may benefit from renaming.
typedef map<Y, string> (hs.type = "HashMap") map_Y_string_2031
