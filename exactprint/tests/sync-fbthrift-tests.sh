#! /bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.

HEADER="// THIS FILE IS COPIED FROM FBTHRIFT, DO NOT MODIFY ITS CONTENTS DIRECTLY\n\
// generated-by : fbcode/common/hs/thrift/exactprint/tests/sync-fbthrift-tests.sh\n\
// source: thrift/compiler/test/fixtures/*\n\
// @"
HEADER="${HEADER}generated"

rsync -am --include='*.thrift' --include='*/' --exclude='*' thrift/compiler/test/fixtures/ common/hs/thrift/exactprint/tests/fbthrift-tests
find common/hs/thrift/exactprint/tests/fbthrift-tests -type f -exec sed -i 1i"$HEADER" {} \;
