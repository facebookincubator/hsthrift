// Copyright (c) Facebook, Inc. and its affiliates.

typedef map<string, string> hstypedef
typedef map<string, string> hsnewtypeann (hs.newtype)

struct HsStruct {
    1: i32 strictann (hs.strict)
    2: i32 lazyann (hs.lazy)
    3: i32 inherit
}
struct HsStrictAnn {
    1: i32 strictann (hs.strict)
    2: i32 lazyann (hs.lazy)
    3: i32 inherit
} (hs.strict)
struct HsLazyAnn {
    1: i32 strictann (hs.strict)
    2: i32 lazyann (hs.lazy)
    3: i32 inherit
} (hs.lazy)
struct HsPrefixAnn {
    1: i32 strictann (hs.strict)
    2: i32 lazyann (hs.lazy)
    3: i32 inherit
} (hs.prefix="structprefix")

union HsUnion {
  1: i32 left
  2: i32 right
}
union HsUnionNonEmptyAnn {
  1: i32 left
  2: i32 right
} (hs.nonempty)

enum HsEnum {
    ONE = 1,
    TWO = 2,
    THREE = 3,
}
enum HsStructNoUnknownAnn {
    ONE = 1,
    TWO = 2,
    THREE = 3,
} (hs.nounknown)
enum HsStructPsuedoenumAnn {
    ONE = 1,
    TWO = 2,
    THREE = 3,
} (hs.psuedoenum)
