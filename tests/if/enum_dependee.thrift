/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "if/enum.thrift"

// Const of typedef'd enum that was imported from another file
typedef enum.PerfectEnum PerfectEnum
const enum.PerfectEnum BOTH_QUALIFIED = enum.PerfectEnum.W;
const PerfectEnum UNQUALIFIED = PerfectEnum.X;
const enum.PerfectEnum TYPE_QUALIFIED = PerfectEnum.Y;
const PerfectEnum IDENT_QUALIFIED = enum.PerfectEnum.Z;
