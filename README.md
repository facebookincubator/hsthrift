# The Haskell Thrift Compiler

[Thrift](https://thrift.apache.org/static/files/thrift-20070401.pdf)
is a serialization and remote procedure call (RPC) framework used for
cross-service communication. Most services at Facebook communicate via
Thrift because it provides a simple, language-agnostic protocol for
communicating with structured data. Thrift can already be used in
programming languages such as C++, Python, and Java using
[fbthrift](https://github.com/facebook/fbthrift).

The **hsthrift** project includes the full collection of tools and
libraries for using Thrift in your own Haskell projects. The [Haskell
Thrift compiler](compiler) generates the Haskell
code needed to communicate with other Thrift services, and the
included [libraries](lib) allow you to build both
Thrift clients and [servers](server) in
Haskell. Haskell Thrift is fully compatible with all other fbthrift
languages, so your Haskell project can freely communicate with other
services no matter what language they are implemented in.

Note: `hsthrift` is compatible with `fbthrift` but not Apache
Thrift clients and servers.

`hsthrift` is licensed under a standard [BSD license](LICENSE).

# Documentation

Build instructions are below.

To use Thrift, see [Getting Started with Thrift in Haskell](docs/README.md).

# Cabal packages

The repository contains the following packages:

* [mangle](common/mangle), a library for mangling C++
  function prototypes into symbol names, useful for using C++ code via
  the Haskell FFI.
* [fb-util](common/util), a collection of utilities
* [folly-clib](folly-clib), the folly C++ library wrapped in a Cabal package
* [thrift-compiler](compiler), the Haskell Thrift compiler
* [thrift-lib](lib), libraries for Thrift clients
* [thrift-tests](tests), a test suite
* [thrift-http](http), Thrift client and server transport (using HTTP)
* [thrift-cpp-channel](cpp-channel), libraries for Thrift clients (using fbthrift)
* [thrift-server](server), libraries for Thrift servers (using fbthrift)

# Building from Hackage

If you only want to use the HTTP transport (not fbthrift) then you can
build the packages directly from Hackage; if not, go to the next
section for instructions to build from the repository. All of the
packages above are on Hackage except for `thrift-cpp-channel` and
`thrift-server`.

```
$ cabal install thrift-compiler thrift-lib thrift-http
```

`thrift-cpp-channel` and `thrift-server` aren't on Hackage because
they depend on fbthrift and can only be built from the repository; see
the next section.

# Building and testing from the repository

The following instructions assume you want to use `fbthrift`. To omit
the `fbthrift` dependency and use only the HTTP transport, follow the
extra instructions marked with "[no fbthrift]".

First install all dependencies (see [Dependencies](#Dependencies) section below).

Note that `hsthrift` requires Cabal version 3.6 or later.

We use the C++ thrift compiler to generate some files needed to build
the Haskell thrift compiler.

[no fbthrift] Omit this step.

``` sh
$ make thrift-cpp
```

[no fbthrift] Edit `cabal.project` to remove `thrift-cpp-channel` and add

```
package thrift-tests
    flags: -fbthrift
```

Next build the Haskell Thrift compiler:

```
$ cabal build exe:thrift-compiler
```

At this point, you should have a working `thrift-compiler`. You can
check that this is indeed the case by running the following
command to process many `.thrift` files used by tests of various
packages in this repository with your freshly built compiler:

``` sh
$ make thrift-hs
```

Finally, we can issue any `cabal build`/`cabal run`/`cabal test`
command to build/run/test a specific component or entire package.
For instance, we could test all the packages with:

``` sh
$ cabal test all

# or, if there's no IPv6 configured on your machine (see below),
# you can borrow the ci configuration or derive your own:
$ cabal test --project-file=ci.cabal.project all
```

## Source distributions

A simple

``` sh
$ cabal sdist <package name>
# or, to generate all sdists
$ cabal sdist all
```

will generate source distribution archives under
`dist-newstyle/sdist/`, all named following the fairly common
`<pkg>-<version>.tar.gz` template.

The Github CI actions make use of these commands to build and test
the packages from source distributions.

## IPv4 vs IPv6

Some tests bring up thrift clients and servers on localhost, but
designate the said localhost differently depending on whether the
code is built to use `::1` (IPv6) or `127.0.0.1` (IPv4). This is
determined by a `tests_use_ipv4` cabal flag in `thrift-lib` and
`thrift-server`. The Github CI actions turn those flags on as can
be seen in the `.cabal.project` files at the root of this repository,
because the Docker environment in which CI actions get executed does
not have IPv6 configured.

# Dependencies

## System dependencies

### Debian / Ubuntu

```
sudo apt install \
    libgoogle-glog-dev \
    libgflags-dev \
    bison \
    flex \
    ninja-build \
    cmake \
    libzstd-dev
```

(TODO: this list is probably incomplete, please file an issue if you
encounter something missing)

### Fedora

```
sudo yum install \
     openssl-devel \
     libevent-devel \
     double-conversion-devel \
     boost-devel \
     cmake \
     ninja-build \
     glog-devel \
     gflags-devel \
     gmock-devel \
     bison \
     flex \
     libatomic \
     libsodium \
     libzstd-devel \
     pcre-devel \
     ncurses-devel \
     fmt-devel \
     gmp-devel
```

Also you may need:

```
export LD_LIBRARY_PATH=/usr/local/lib:
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
```

## Other dependencies

hsthrift also depends on some other C++ libraries that are not
typically packaged for Linux. We've provided a script to fetch them
from github, and build and install them locally. Run the following
commands to clone the repos and build and install the dependencies
(pass e.g. `--threads 8` to build in parallel. Default is 4):

```
./new_install_deps.sh
```

[no fbthrift] Instead use:

```
./new_install_deps.sh --no-fbthrift
```

The dependencies are:

* [folly](https://github.com/facebook/folly), a library of general C++ utilities
* [libfmt](https://github.com/fmtlib/fmt/releases) v8
* [fbthrift](https://github.com/facebook/fbthrift), Facebook's Thrift compiler and support libraries.
* Other C++ libraries required by fbthrift: [rsocket-cpp](https://github.com/rsocket/rsocket-cpp), [fizz](https://github.com/facebookincubator/fizz), [wangle](https://github.com/facebook/wangle).

After running `new_install_deps.sh`, set your env variables to pick up the new libraries and binaries:

```
export LD_LIBRARY_PATH=$HOME/.hsthrift/lib:
export PKG_CONFIG_PATH=$HOME/.hsthrift/lib/pkgconfig
export PATH=$PATH:$HOME/.hsthrift/bin
```

```
make all
```

and test the installation with:
```
cabal test all
```
