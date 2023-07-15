/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace hs "Thrift.Protocol"

enum ApplicationExceptionType {
  Unknown = 0,
  UnknownMethod = 1,
  InvalidMessageType = 2,
  WrongMethodType = 3,
  BadSequenceId = 4,
  MissingResult = 5,
  InternalError = 6,
  ProtocolError = 7,
  InvalidTransform = 8,
  InvalidProtocol = 9,
  UnsupportedClientType = 10,
  Loadshedding = 11,
  Timeout = 12,
  InjectedFailure = 13,
}

exception ApplicationException {
  1: string message;
  2: ApplicationExceptionType type;
}
