#! /bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

HEADER="// THIS FILE IS COPIED FROM FBTHRIFT, DO NOT MODIFY ITS CONTENTS DIRECTLY\n\
// generated-by : fbcode/common/hs/thrift/exactprint/tests/sync-fbthrift-tests.sh\n\
// source: thrift/compiler/test/fixtures/*\n\
// @"
HEADER="${HEADER}generated"

rsync -am --include='*.thrift' --include='*/' --exclude='*' thrift/compiler/test/fixtures/ common/hs/thrift/exactprint/tests/fbthrift-tests
find common/hs/thrift/exactprint/tests/fbthrift-tests -type f -exec sed -i 1i"$HEADER" {} \;
