// Copyright (c) Facebook, Inc. and its affiliates.

include "test/if/math.thrift"

service Echoer extends math.Calculator {
  string echo (1: string input)
}
