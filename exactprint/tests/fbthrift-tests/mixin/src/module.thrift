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

include "thrift/annotation/thrift.thrift"

struct Mixin1 {
  1: string field1;
}

struct Mixin2 {
  @thrift.Mixin
  1: Mixin1 m1;
  2: optional string field2;
}

struct Mixin3Base {
  1: string field3;
}

typedef Mixin3Base Mixin3

struct Foo {
  1: string field4;
  @thrift.Mixin
  2: Mixin2 m2;
  @thrift.Mixin
  3: Mixin3 m3;
}
