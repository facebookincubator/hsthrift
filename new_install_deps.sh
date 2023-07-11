#!/usr/bin/env bash

# Copyright (c) Facebook, Inc. and its affiliates.

set -e

BUILDER=./build.sh
THREADS=4

#
# These are required source deps not satisifiable with common package systems
# in topological order. You can pass additional deps on the command line.
#
BASE_DEPS="fmt folly"
FBTHRIFT_DEPS="fizz wangle fbthrift"
EXTRA_DEPS=""

while [ "$#" -gt 0 ]; do
    case "$1" in
        --threads) THREADS="$2"; shift; shift;;
        --no-fbthrift) FBTHRIFT_DEPS=""; shift;;
        *) EXTRA_DEPS+=" $1"; shift;;
    esac
done

DEPS="$BASE_DEPS $FBTHRIFT_DEPS $EXTRA_DEPS"

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
if [ -d "${INSTALL_PREFIX}/lib64" ]; then
    echo "export LD_LIBRARY_PATH=${INSTALL_PREFIX}/lib:${INSTALL_PREFIX}/lib64"
    echo "export PKG_CONFIG_PATH=${INSTALL_PREFIX}/lib/pkgconfig:${INSTALL_PREFIX}/lib64/pkgconfig"
else
    echo "export LD_LIBRARY_PATH=${INSTALL_PREFIX}/lib"
    echo "export PKG_CONFIG_PATH=${INSTALL_PREFIX}/lib/pkgconfig"
fi
echo "export PATH=\$PATH:${INSTALL_PREFIX}/bin"
