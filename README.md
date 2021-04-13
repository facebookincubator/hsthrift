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
* [thrift-compiler](compiler), the Haskell Thrift compiler
* [thrift-lib](lib), libraries for Thrift clients
* [thrift-server](server), libraries for Thrift servers
* [thrift-tests](tests), a test suite

# Building and testing

First install all dependencies (see [Dependencies](#Dependencies) section below).

Then, build a local checkout of `cabal-install`'s master branch
using commit `f5f8d933db229d30e6fc558f5335f0a4e85d7d44` or
newer. The aforementioned commit will be available in the
3.6 release of `Cabal` and `cabal-install`.

We then use the C++ thrift compiler to generate some files needed
to build the Haskell thrift compiler, which we can do immediately
afterwards.

``` sh
$ make thrift-cpp
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

sudo apt install \
    libgoogle-glog-dev \
    libgflags-dev \
    libmysqlclient-dev \
    bison \
    flex \
    libzstd-dev

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
     glog-devel \
     gflags-devel \
     gmock-devel \
     bison \
     flex \
     libatomic \
     libsodium \
     libzstd-devel \
     pcre-devel \
     community-mysql-devel
```

Also you may need:

```
export LD_LIBRARY_PATH=/usr/local/lib:
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
```

## Other dependencies

hsthrift also depends on:

* [folly](https://github.com/facebook/folly), a library of general C++ utilities
* [fbthrift](https://github.com/facebook/fbthrift), Facebook's Thrift compiler and support libraries.
* Other C++ libraries required by fbthrift: [rsocket-cpp](https://github.com/rsocket/rsocket-cpp), [fizz](https://github.com/facebookincubator/fizz), [wangle](https://github.com/facebook/wangle).

These are typically not packaged by Linux distributions, so we have to
build and install them manually from their github repos.  We've
provided a script in the hsthrift repository, `install_deps.sh` to do
that.  Run the following commands to clone the repos and build and
install the dependencies in `/usr/local`:

```
mkdir deps
cd deps
../install_deps --sudo
```

Note that you can repeat `../install_deps --sudo --clean` in the
future to update, build and install all the dependencies
again.

These library repositories must be checked out with a consistent
tag. They are all tagged regularly with tags like
`v2021.01.11.00`. The `install_deps.sh` script will find the most
recent tag and update all the repos to the same tag.
