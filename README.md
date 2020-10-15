# Building

First install all dependencies (see "Dependencies" below).

Build a local `cabal-install` that contains this patch: https://github.com/haskell/cabal/pull/6295

Change the `CABAL` variable in `Makefile` to point to your local `cabal-install`

Build the compiler:

```
make compiler
```

Use the compiler to process files needed by the libraries:

```
make thrift
```

Finally build the server, which will build the rest:

```
make server
```

# Dependencies

The following instructions worked on Ubuntu 16.04, please update as
necessary for other distros/versions.

TODO: automate all this with scripts.

## glog

required by: various things

sudo apt install libgoogle-glog-dev

## gflags

sudo apt install libgflags-dev

## folly

Required by: fbthrift, fb-util

NOTE: make sure that folly, fizz, wangle are all checked out at the same tag. e.g. I used v2019-10-07-00

```
git clone git@github.com:simonmar/folly.git
```

(my fork has a couple of changes)

edit `CMake/libfolly.pc.in` to set:

Version: ﻿2020.07.10.00﻿

(otherwise Cabal complains that it can't parse the version "master" in
the `pkg-config --modversion` output)

follow instructions in `README.md` except for:

`cmake -DBUILD_SHARED_LIBS=ON ..`

(because we always build dynamic versions of Haskell libs, for GHCi,
and otherwise this will fail saying it needs -fPIC)

```
sudo make install # puts files under /usr/local
```

## bison/flex

required by: fbthrift

```
sudo apt install bison flex
```

## rsocket / yarpl

required by: fbthrift

```
git clone ﻿git@github.com﻿:rsocket/rsocket-cpp.git
```

follow build instructions, but add to cmake: `-DBUILD_SHARED_LIBS=ON`

## fizz

required by: fbthrift

follow instructions at ﻿https://github.com/facebookincubator/fizz﻿

```
cmake ../fizz -DBUILD_SHARED_LIBS=ON
```

## wangle

required by: fbthrift

follow instructions at ﻿`https://github.com/facebook/wangle﻿`

```
cd wangle
cmake . -DBUILD_SHARED_LIBS=ON
```

## zstd

required by: fbthrift, rocksdb

```
sudo apt install libzstd-dev
```

## fmt

required by: fbthrift

﻿https://github.com/fmtlib/fmt﻿

NB: checkout version 6.1.1 (the one currently in tp2), master doesn’t necessarily work

Apply this patch (TODO: not sure if this is actually needed (2020-07-10)):
```
diff --git a/CMakeLists.txt b/CMakeLists.txt
index f872e3c9..bf684171 100644
--- a/CMakeLists.txt
+++ b/CMakeLists.txt
@@ -161,6 +161,8 @@ endif ()
 add_library(fmt ${FMT_SOURCES} ${FMT_HEADERS} README.rst ChangeLog.rst)
 add_library(fmt::fmt ALIAS fmt)
 
+set_target_properties(fmt PROPERTIES POSITION_INDEPENDENT_CODE True)
+
 if (FMT_WERROR)
   target_compile_options(fmt PRIVATE ${WERROR_FLAG})
 endif ()
```

build & install as per instructions

## fbthrift

```
git clone https://github.com/facebook/fbthrift
cd fbthrift/build
cmake .. -DBUILD_SHARED_LIBS=ON
make -j2 && sudo make install
```

## rocksdb

required by: Glean

```
sudo apt install librocksdb-dev
```

## MySQL client lib

required by: some modules in fb-util

```
sudo apt install libmysqlclient-dev
```
