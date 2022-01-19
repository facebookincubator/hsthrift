#!/usr/bin/env bash

SCRIPT_DIR=$(dirname "${BASH_SOURCE[0]}")
GETDEPS_PATHS=(
    "$SCRIPT_DIR/build/fbcode_builder/getdeps.py"
    "$SCRIPT_DIR/../../../opensource/fbcode_builder/getdeps.py"
)

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
