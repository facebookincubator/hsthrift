/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

exception NotFound {
  1: string key;
}

service HashMapService {
  void put(1: string key, 2: binary value);

  binary get(1: string key) throws (1: NotFound ex);
}
