/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "if/foo.thrift"

typedef foo.Bar NewBar
typedef list<foo.HsString> (hs.type = "Vector") HsStringVector
