// THIS FILE IS COPIED FROM FBTHRIFT, DO NOT MODIFY ITS CONTENTS DIRECTLY
// generated-by : fbcode/common/hs/thrift/exactprint/tests/sync-fbthrift-tests.sh
// source: thrift/compiler/test/fixtures/*
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

package "facebook.com/thrift/compiler/test/fixtures/default_values_rectification"

struct EmptyStruct {}

struct TestStruct {
  1: i32 unqualified_int_field = 0;

  2: bool unqualified_bool_field = false;

  3: list<i32> unqualified_list_field = [];

  4: EmptyStruct unqualified_struct_field = {};

  5: optional i32 optional_int_field = 42;

  6: optional bool optional_bool_field = true;

  7: optional list<i32> optional_list_field = [1, 2];

  8: optional EmptyStruct optional_struct_field = {};
}
