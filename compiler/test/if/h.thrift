/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "thrift/annotation/haskell.thrift"

// Test structured annotations produce the same results
// as the unstructured equivalents in d.thrift

@haskell.Newtype
typedef map<string, string> hsnewtypeann_sa

@haskell.Strict
struct HsStrictAnn_SA {
  1: i32 strictann;
  @haskell.Lazy
  2: i32 lazyann;
  3: i32 inherit;
}

@haskell.Lazy
struct HsLazyAnn_SA {
  @haskell.Strict
  1: i32 strictann;
  2: i32 lazyann;
  3: i32 inherit;
}

struct HsFieldAnns_SA {
  @haskell.Strict
  1: i32 strictann;
  @haskell.Lazy
  2: i32 lazyann;
  3: i32 inherit;
}

@haskell.NonEmpty
union HsUnionNonEmptyAnn_SA {
  1: i32 left;
  2: i32 right;
}

@haskell.NoUnknown
enum HsEnumNoUnknownAnn_SA {
  ONE = 1,
  TWO = 2,
  THREE = 3,
}

@haskell.PseudoEnum
enum HsEnumPseudoenumAnn_SA {
  ONE = 1,
  TWO = 2,
  THREE = 3,
}

@haskell.PseudoEnum{value = "thriftenum"}
enum HsEnumPseudoenumThriftAnn_SA {
  ONE = 1,
  TWO = 2,
  THREE = 3,
}

struct HsFoo_SA {
  @haskell.Hidden
  1: i64 bar;
  2: i32 baz;
}

@haskell.Prefix{name="ps_"}
struct PrefixedStruct_SA {
  1: i64 foo;
  2: i32 bar;
}

@haskell.Type{name="HashMap"}
typedef map<string, string> map_string_string_sa

@haskell.Type{name="Int"}
typedef i64 int_sa

@haskell.Type{name="String"}
typedef string string_sa
