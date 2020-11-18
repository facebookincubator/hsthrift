#!/usr/bin/env sh

set -e

git clone https://github.com/facebook/folly.git
cd folly
# PROJ_TAG="v2020.07.13.00"
PROJ_TAG=$(git tag | sort -r | head -1)
echo "Using tag: $PROJ_TAG"
git checkout $PROJ_TAG
# curl -s -o simonspatch.diff https://github.com/simonmar/folly/commit/9b1e7eaf2513ecd51e42336f950b4b79d816ac95.diff && git apply simonspatch.diff
mkdir _build
cd _build
# cmake -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=off -DBUILD_TESTS=off ..
cmake -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=off -DBUILD_TESTS=off ..
make -j2 && make install
cd ../..
rm -rf folly/

git clone https://github.com/rsocket/rsocket-cpp.git
cd rsocket-cpp
git checkout $PROJ_TAG
mkdir _build
cd _build
# cmake -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=off -DBUILD_TESTS=off ..
cmake -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=off -DBUILD_TESTS=off ..
make -j2 && make install
cd ../..
rm -rf rsocket-cpp/

git clone https://github.com/facebookincubator/fizz.git
cd fizz
git checkout $PROJ_TAG
mkdir _build
cd _build
cmake -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=off -DBUILD_TESTS=off ../fizz
make -j2 && make install
cd ../..
rm -rf fizz/

git clone https://github.com/facebook/wangle.git
cd wangle
git checkout $PROJ_TAG
mkdir _build
cd _build
cmake -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=off -DBUILD_TESTS=off ../wangle
make -j2 && make install
cd ../../
rm -rf wangle/

git clone https://github.com/facebook/fbthrift.git
cd fbthrift
git checkout $PROJ_TAG
mkdir _build
cd _build
cmake -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=off -DBUILD_TESTS=off ..
make -j2 && make install
cd ../..
rm -rf fbthrift/
