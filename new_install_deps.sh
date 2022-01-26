#!/usr/bin/env bash

# Copyright (c) Facebook, Inc. and its affiliates.

#
# These are source deps not satisifiable with common package systems
# in topological order
#
DEPS="folly fizz wangle fbthrift"

set -e

BUILDER=./build.sh

# default library and bin install path
if [ -z "${INSTALL_PREFIX}" ]; then
    INSTALL_PREFIX="${HOME}/.hsthrift"
fi

build() {
    ${BUILDER} build --no-deps --install-dir="$INSTALL_PREFIX" "$1"
}

# build in order
for dep in $DEPS; do
    build $dep
done

# add these to your environment
echo "export LD_LIBRARY_PATH=${INSTALL_PREFIX}/lib"
echo "export PKG_CONFIG_PATH=${INSTALL_PREFIX}/lib/pkgconfig"
echo "export PATH="'$PATH'":${INSTALL_PREFIX}/bin"
