#!/usr/bin/env bash

# Copyright (c) Facebook, Inc. and its affiliates.

# Download, build and install dependencies for hsthrift
#
# Flags:
#   --sudo       Use 'sudo make install' (necessary unless you're root)
#   --nuke       Remove each project after building & installing it
#   --clean      Remove old build artifacts before building
#   --threads N  Pass -jN to make

set -e

NUKE=NO
SUDO=
CLEAN=NO
THREADS=2
BUILD_SUBDIR=_build

while [ "$#" -gt 0 ]; do
    case "$1" in
        --nuke) NUKE=YES;;
        --sudo) SUDO=sudo;;
        --clean) CLEAN=YES;;
        --threads) THREADS="$2"; shift;;
        *)
            echo "syntax: install_deps.sh [--nuke] [--sudo] [--clean] [--threads N]"
            exit 1;;
    esac
    shift
done

clone() {
    dir="$1"
    url="$2"
    rev="$3"
    if [ -d "$dir" ]; then
        (cd "$dir" && git fetch)
    else
        git clone "$url" "$dir"
    fi
    if [ "$rev" != "" ]; then
        (cd "$dir" && git checkout "$rev")
    fi
}

build() {
    dir="$1"
    subdir="$2"
    cd "$dir"
    [ "$CLEAN" = "NO" ] || rm -rf "$BUILD_SUBDIR"
    mkdir -p "$BUILD_SUBDIR"
    cd "$BUILD_SUBDIR"
    # cmake -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=off -DBUILD_TESTS=off ..
    cmake -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=off -DBUILD_TESTS=off ../"$subdir"
    make -j"$THREADS" && $SUDO make install
    cd ../..
    [ "$NUKE" = "NO" ] || rm -rf "$dir"
}

clone folly https://github.com/facebook/folly.git
PROJ_TAG=$(cd folly && git tag | sort -r | head -1)
echo "Using tag: $PROJ_TAG"
(cd folly && git checkout "$PROJ_TAG")
build folly .

clone rsocket-cpp https://github.com/rsocket/rsocket-cpp.git "$PROJ_TAG"
build rsocket-cpp .

clone fizz https://github.com/facebookincubator/fizz.git "$PROJ_TAG"
build fizz fizz

clone wangle https://github.com/facebook/wangle.git "$PROJ_TAG"
build wangle wangle

clone fbthrift https://github.com/facebook/fbthrift.git "$PROJ_TAG"
build fbthrift .
