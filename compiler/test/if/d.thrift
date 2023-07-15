/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef map<string, string> hstypedef
typedef map<string, string> hsnewtypeann (hs.newtype)

struct HsStruct {
  1: i32 strictann (hs.strict);
  2: i32 lazyann (hs.lazy);
  3: i32 inherit;
}
struct HsStrictAnn {
  1: i32 strictann (hs.strict);
  2: i32 lazyann (hs.lazy);
  3: i32 inherit;
} (hs.strict)
struct HsLazyAnn {
  1: i32 strictann (hs.strict);
  2: i32 lazyann (hs.lazy);
  3: i32 inherit;
} (hs.lazy)
struct HsPrefixAnn {
  1: i32 strictann (hs.strict);
  2: i32 lazyann (hs.lazy);
  3: i32 inherit;
} (hs.prefix = "structprefix")

union HsUnion {
  1: i32 left;
  2: i32 right;
}
union HsUnionNonEmptyAnn {
  1: i32 left;
  2: i32 right;
} (hs.nonempty)

enum HsEnum {
  ONE = 1,
  TWO = 2,
  THREE = 3,
}
enum HsEnumEmpty {
}
enum HsEnumNoUnknownAnn {
  ONE = 1,
  TWO = 2,
  THREE = 3,
} (hs.nounknown)
enum HsEnumEmptyNoUnknownAnn {
} (hs.nounknown)
enum HsEnumPseudoenumAnn {
  ONE = 1,
  TWO = 2,
  THREE = 3,
} (hs.pseudoenum)
enum HsEnumDuplicatedPseudoenumAnn {
  ONE = 1,
  TWO = 2,
  THREE = 3,
} (hs.pseudoenum)
enum HsEnumEmptyPseudoenumAnn {
} (hs.pseudoenum)
enum HsEnumPseudoenumThriftAnn {
  ONE = 1,
  TWO = 2,
  THREE = 3,
} (hs.pseudoenum = "thriftenum")
enum HsEnumEmptyPseudoenumThriftAnn {
} (hs.pseudoenum = "thriftenum")

struct HsStructOfComplexTypes {
  1: HsStruct a_struct;
  2: HsUnion a_union;
  3: HsEnum an_enum;
  4: HsEnumPseudoenumAnn a_pseudoenum;
  5: HsEnumPseudoenumThriftAnn a_thrift_pseudoenum;
}
