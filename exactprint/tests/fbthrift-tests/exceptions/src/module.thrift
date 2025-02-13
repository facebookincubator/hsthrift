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

namespace java.swift test.fixtures.exceptions

include "thrift/annotation/thrift.thrift"
include "thrift/annotation/python.thrift"

transient server exception Fiery {
  @thrift.ExceptionMessage
  1: required string message;
}

safe stateful exception Serious {
  @thrift.ExceptionMessage
  @python.Name{name = "not_sonnet"}
  1: optional string sonnet;
}

client exception ComplexFieldNames {
  1: string error_message;
  @thrift.ExceptionMessage
  2: string internal_error_message;
}

exception CustomFieldNames {
  1: string error_message;
  @thrift.ExceptionMessage
  2: string internal_error_message (java.swift.name = "internalGreatMessage");
}

exception ExceptionWithPrimitiveField {
  @thrift.ExceptionMessage
  1: string message;
  2: i32 error_code;
}

exception ExceptionWithStructuredAnnotation {
  @thrift.ExceptionMessage
  1: string message_field;
  2: i32 error_code;
}

service Raiser {
  void doBland();
  void doRaise() throws (1: Banal b, 2: Fiery f, 3: Serious s);
  string get200();
  string get500() throws (1: Fiery f, 2: Banal b, 3: Serious s);
}

safe permanent client exception Banal {}
