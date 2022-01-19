#!/usr/bin/env bash

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
    export CXXFLAGS=-march=corei7
fi

# N.B. we always need shared libs, but this only checks build is the first arg
if [ "$1" = "build" ]; then
    set -- "$@" --extra-cmake-defines='{"BUILD_SHARED_LIBS": "ON", "BUILD_EXAMPLES": "off", "BUILD_TESTS": "off", "CMAKE_INSTALL_RPATH_USE_LINK_PATH": "TRUE", "EVENT__BUILD_SHARED_LIBRARIES": "ON"}'
fi

for getdeps in "${GETDEPS_PATHS[@]}"; do
    if [[ -x "$getdeps" ]]; then
            exec "$getdeps" "$@"
    fi
done
echo "Could not find getdeps.py" >&2
exit 1
