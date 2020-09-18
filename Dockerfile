FROM ubuntu:20.04
ENV TZ=Europe/London
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt-get update
RUN apt-get install -y \
    g++ \
    cmake \
    bison flex \
    git cmake \
    libzstd-dev \
    libboost-all-dev \
    libevent-dev \
    libdouble-conversion-dev \
    libgoogle-glog-dev \
    libgflags-dev \
    libiberty-dev \
    liblz4-dev \
    liblzma-dev \
    libsnappy-dev \
    make \
    zlib1g-dev \
    binutils-dev \
    libjemalloc-dev \
    libssl-dev \
    pkg-config \
    libunwind-dev \
    libsodium-dev \
    curl \
    libpcre3-dev \
    libmysqlclient-dev \
    libfftw3-dev

RUN git clone https://github.com/fmtlib/fmt.git && cd fmt && git checkout 6.1.1 && mkdir _build && cd _build && cmake -DBUILD_SHARED_LIBS=ON .. && make -j2 && make install
RUN rm -rf fmt/

RUN git clone https://github.com/facebook/folly.git && cd folly && git checkout v2020.07.13.00 && curl -s -L https://github.com/simonmar/folly/commit/9b1e7eaf2513ecd51e42336f950b4b79d816ac95.diff -o folly.patch && git apply folly.patch
RUN cd folly && mkdir _build && cd _build && cmake -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=off -DBUILD_TESTS=off .. && make -j2 && make install
RUN rm -rf folly/

RUN git clone https://github.com/rsocket/rsocket-cpp.git && cd rsocket-cpp && git checkout v2020.07.13.00 && mkdir _build && cd _build && cmake -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=off -DBUILD_TESTS=off .. && make -j2 && make install
RUN rm -rf rsocket-cpp/

RUN git clone https://github.com/facebookincubator/fizz.git && cd fizz && git checkout v2020.07.13.00 && mkdir _build && cd _build && cmake -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=off -DBUILD_TESTS=off ../fizz && make -j2 && make install
RUN rm -rf fizz/

RUN git clone https://github.com/facebook/wangle.git && cd wangle && git checkout v2020.07.13.00 && mkdir _build && cd _build && cmake -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=off -DBUILD_TESTS=off ../wangle && make -j2 && make install
RUN rm -rf wangle/

RUN git clone https://github.com/facebook/fbthrift.git && cd fbthrift && git checkout v2020.07.13.00 && mkdir _build && cd _build && cmake -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=off -DBUILD_TESTS=off .. && make -j2 && make install
RUN rm -rf fbthrift/

# install vanilla ghc/cabal
RUN apt-get install -y software-properties-common
RUN apt-add-repository ppa:hvr/ghc
RUN apt-get update
RUN apt-get install -y ghc-8.4.4 cabal-install-3.4
ENV PATH=/opt/ghc/bin:$PATH
RUN cabal update

# build a post-3.4 cabal HEAD which includes support for the new
# 'hsc2hs-options' field.
RUN git clone https://github.com/haskell/cabal.git && cd cabal && git checkout f5f8d933db229d30e6fc558f5335f0a4e85d7d44
RUN cd cabal && sed -i 's/3.5.0.0/3.6.0.0/' */*.cabal
RUN cd cabal && cabal install cabal-install/ --allow-newer=Cabal-QuickCheck:Cabal --allow-newer=Cabal-described:Cabal --allow-newer=Cabal-tree-diff:Cabal --allow-newer=cabal-install:Cabal --allow-newer=cabal-install-solver:Cabal
RUN rm -rf cabal/
ENV PATH=/root/.cabal/bin:$PATH
RUN cabal --version
