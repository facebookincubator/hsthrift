#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

set -e

BUILDER=./build.sh
THREADS=4

#
# These are required source deps not satisifiable with common package systems
# in topological order. You can pass additional deps on the command line.
#
BASE_DEPS="fmt fast_float folly"
FBTHRIFT_DEPS="fizz wangle mvfst fbthrift"
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


if [ ! -d "${INSTALL_PREFIX}/bin" ]; then
    mkdir -p ${INSTALL_PREFIX}/bin
fi

# getdeps.py assumes fbpython exists
if [ ! -L "${INSTALL_PREFIX}/bin/fbpython" ]; then
    ln -s $(which python3) ${INSTALL_PREFIX}/bin/fbpython
fi
export PATH=${PATH}:${INSTALL_PREFIX}/bin

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
