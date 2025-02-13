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

struct EchoRequest {
  1: string text;
}

struct EchoResponse {
  1: string text;
}

@cpp.GenerateDeprecatedHeaderClientMethods
service HeaderClientMethodsAnnotationOnService {
  EchoResponse echo(1: EchoRequest request);
  EchoResponse echo_2(1: EchoRequest request);
}

service HeaderClientMethodsAnnotationOnFunction {
  @cpp.GenerateDeprecatedHeaderClientMethods
  EchoResponse echo(1: EchoRequest request);
  EchoResponse echo_2(1: EchoRequest request);
}
