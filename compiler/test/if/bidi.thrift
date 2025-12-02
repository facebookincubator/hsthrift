/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace cpp2 apache.thrift.detail.test

exception BiDiSinkException {
  1: string message;
}

exception BiDiStreamException {
  1: string message;
}

exception BiDiMethodException {
  1: string message;
}

service TestBiDiService {
  // @lint-ignore THRIFTCHECKS new unreleased feature
  sink<string>, stream<string> echo();

  // @lint-ignore THRIFTCHECKS new unreleased feature
  string, sink<string>, stream<string> echoWithResponse(1: string initial);

  // @lint-ignore THRIFTCHECKS new unreleased feature
  sink<i64>, stream<i64> intStream();

  // @lint-ignore THRIFTCHECKS new unreleased feature
  sink<i64 throws (1: BiDiSinkException sinkEx)>, stream<
    i64 throws (1: BiDiStreamException sinkEx)
  > canThrow() throws (1: BiDiMethodException methodEx);
}
