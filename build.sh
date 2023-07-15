#!/usr/bin/env bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

SCRIPT_DIR=$(dirname "${BASH_SOURCE[0]}")
GETDEPS_PATHS=(
    "$SCRIPT_DIR/build/fbcode_builder/getdeps.py"
    "$SCRIPT_DIR/../../../opensource/fbcode_builder/getdeps.py"
)

# folly requires certain architecture flags to be used consistently
# when compiling the library and the application code. Getting this
# wrong results in a link error (fortunately), see
#   folly/container/detail/F14IntrinsicsAvailability.h
# In Glean we use some AVX2 intrinsics, so we have to pick a compatible
# architecture here for compiling folly.
arch=$(uname -m)
if [ "$arch" == x86_64 ] ; then
    export CXXFLAGS=-march=haswell
fi

# when CMAKE_LIBRARY_ARCHITECTURE is not set folly defaults to sse4/x86_64 flavor
# don't do that on arm
if [ "$arch" == aarch64 ] ; then
    APPEND_EXTRA_CMAKE_DEFINES='"CMAKE_LIBRARY_ARCHITECTURE": "aarch64",'
fi

# if the C++ compiler is clang, we need fbthrift to build with -fsized-deallocation
# this is a janky way to make that happen until we work out how to patch fbthrift
clang=$(cc --version | sed -n 's/^.*\(clang\).*$/\1/p')
if [ -n "$clang" ] ; then
    export CXXFLAGS="$CXXFLAGS -fsized-deallocation"
fi

# N.B. we always need shared libs, but this only checks build is the first arg
if [ "$1" = "build" ]; then
    set -- "$@" --extra-cmake-defines='{'"${APPEND_EXTRA_CMAKE_DEFINES}"'"BUILD_SHARED_LIBS": "ON", "BUILD_EXAMPLES": "off", "BUILD_TESTS": "off", "CMAKE_INSTALL_RPATH_USE_LINK_PATH": "TRUE", "EVENT__BUILD_SHARED_LIBRARIES": "ON", "BOOST_LINK_STATIC": "OFF"}'
fi

for getdeps in "${GETDEPS_PATHS[@]}"; do
    if [[ -x "$getdeps" ]]; then
            exec "$getdeps" "$@"
    fi
done
echo "Could not find getdeps.py" >&2
exit 1
