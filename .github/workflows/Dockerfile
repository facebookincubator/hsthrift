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
    ninja-build \
    bison flex \
    git \
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
    libfftw3-dev \
    libgmp-dev \
    libtinfo-dev \
    libgmock-dev

# install ghcup
ARG GHCUP_VERSION=0.1.17.4
RUN curl --proto '=https' --tlsv1.2 -sSf https://downloads.haskell.org/~ghcup/$GHCUP_VERSION/x86_64-linux-ghcup-$GHCUP_VERSION > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup

ENV PATH=/root/.ghcup/bin:/root/.hsthrift/bin:$PATH
ENV LD_LIBRARY_PATH=/usr/local/lib:/root/.hsthrift/lib:/usr/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH
ENV PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/root/.hsthrift/lib/pkgconfig:$PKG_CONFIG_PATH
