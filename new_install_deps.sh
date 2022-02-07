#!/usr/bin/env bash

# Copyright (c) Facebook, Inc. and its affiliates.

set -e

BUILDER=./build.sh
THREADS=4

#
# These are required source deps not satisifiable with common package systems
# in topological order. You can pass additional deps on the command line.
#
DEPS="fmt folly fizz wangle fbthrift"

while [ "$#" -gt 0 ]; do
    case "$1" in
        --threads) THREADS="$2"; shift; shift;;
        *) DEPS+=" $1"; shift;;
    esac
done

# default library and bin install path
if [ -z "${INSTALL_PREFIX}" ]; then
    INSTALL_PREFIX="${HOME}/.hsthrift"
fi

# build in order
for dep in $DEPS; do
    ${BUILDER} build --no-deps --install-dir "${INSTALL_PREFIX}" \
        --num-jobs "${THREADS}" "$dep"
done

# add these to your environment
echo "export LD_LIBRARY_PATH=${INSTALL_PREFIX}/lib"
echo "export PKG_CONFIG_PATH=${INSTALL_PREFIX}/lib/pkgconfig"
echo "export PATH=\$PATH:${INSTALL_PREFIX}/bin"
