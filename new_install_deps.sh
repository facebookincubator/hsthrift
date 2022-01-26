#!/usr/bin/env bash

# Copyright (c) Facebook, Inc. and its affiliates.

#
# These are source deps not satisifiable with common package systems
# in topological order
#
DEPS="fizz folly wangle fbthrift"

set -e

BUILDER=./build.sh

build() {
    ${BUILDER} build --no-deps --install-dir="$INSTALL_PREFIX" "$1"
}

# build in order
for dep in $DEPS; do
    build $dep
done
