// THIS FILE IS COPIED FROM FBTHRIFT, DO NOT MODIFY ITS CONTENTS DIRECTLY
// generated-by : fbcode/common/hs/thrift/exactprint/tests/sync-fbthrift-tests.sh
// source: xplat/thrift/compiler/test/fixtures/*
// @generated
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

include "thrift/annotation/cpp.thrift"

cpp_include "<deque>"
cpp_include "<unordered_map>"
cpp_include "<unordered_set>"
cpp_include "<folly/container/F14Set.h>"
cpp_include "<folly/FBString.h>"
cpp_include "<folly/container/F14Map.h>"
cpp_include "<folly/small_vector.h>"
cpp_include "<thrift/test/python_capi/indirection.h>"

package "test.dev/fixtures/python_capi"

@cpp.Type{name = "folly::IOBuf"}
typedef binary IOBuf

@cpp.Type{template = "folly::small_vector"}
typedef list<IOBuf> small_vector_iobuf

@cpp.Type{template = "folly::fbvector"}
typedef list<string> fbvector_string

@cpp.Type{template = "folly::fbvector"}
typedef list<fbvector_string> fbvector_fbvector_string

/**
 * Vector-like container types that don't work in thrift-cpp2:
 * - std::array -> requires fixed size, fails cpp2 compilation
 * - std::stack -> no push_back
 * - std::forward_list -> no size()
 * Containers we can support but don't currently work:
 * - std::list (need to switch constructor to forward iterator)
 */
struct TemplateLists {
  // @cpp.Type{template = "std::forward_list"}
  // 2: optional list<i64> intz;
  @cpp.Type{template = "std::vector"}
  1: optional list<string> std_string;
  @cpp.Type{template = "std::deque"}
  2: list<binary> deque_string;
  3: small_vector_iobuf small_vector_iobuf;
  @cpp.Type{template = "folly::small_vector"}
  4: list<fbvector_string> nested_small_vector;
  @cpp.Type{template = "folly::fbvector"}
  5: list<fbvector_fbvector_string> small_vector_tensor;
  // @cpp.Type{template = "std::list"}
  // 6: list<string> list_string;
} (cpp.noncomparable)

struct TemplateSets {
  @cpp.Type{template = "std::set"}
  1: set<string> std_set;
  @cpp.Type{template = "std::unordered_set"}
  2: set<string> std_unordered;
  @cpp.Type{template = "folly::F14FastSet"}
  3: set<string> folly_fast;
  @cpp.Type{template = "folly::F14NodeSet"}
  4: set<string> folly_node;
  @cpp.Type{template = "folly::F14ValueSet"}
  5: set<string> folly_value;
  @cpp.Type{template = "folly::F14VectorSet"}
  6: set<string> folly_vector;
  @cpp.Type{template = "folly::sorted_vector_set"}
  7: set<string> folly_sorted_vector;
}

struct TemplateMaps {
  @cpp.Type{template = "std::map"}
  1: map<string, string> std_map;
  @cpp.Type{template = "std::unordered_map"}
  2: map<string, string> std_unordered;
  @cpp.Type{template = "folly::F14FastMap"}
  3: map<string, string> folly_fast;
  @cpp.Type{template = "folly::F14NodeMap"}
  4: map<string, string> folly_node;
  @cpp.Type{template = "folly::F14ValueMap"}
  5: map<string, string> folly_value;
  @cpp.Type{template = "folly::F14VectorMap"}
  6: map<string, string> folly_vector;
  @cpp.Type{template = "folly::sorted_vector_map"}
  7: map<string, string> folly_sorted_vector;
}

struct TWrapped {
  1: string fieldA;
  2: binary fieldB;
}

typedef list<TWrapped> ListOfWrapped

// Should not be marshaled
struct IndirectionA {
  1: ListOfWrapped lst;
}

@cpp.Type{template = "std::vector"}
typedef list<TWrapped> VecOfWrapped

// Should also not be marshaled
struct IndirectionB {
  1: VecOfWrapped lst;
}

typedef ListOfWrapped ListOfWrappedAlias

// Should also not be marshaled
struct IndirectionC {
  1: ListOfWrappedAlias lst;
}
