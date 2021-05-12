#
# This produces the image that is used for running the github actions
# CI jobs.
#
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

# install vanilla ghc/cabal
RUN apt-get update
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
ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
