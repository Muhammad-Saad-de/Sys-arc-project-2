FROM ubuntu:noble-20251013

ARG UUID=1000
ARG GUID=1000
ENV USERNAME=ubuntu

ENV SCALA_VERSION=2.13.17
ENV ALMOND_VERSION=0.14.4

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
        build-essential \
        ninja-build \
        cmake \
        ca-certificates-java \
        curl \
        graphviz \
        openjdk-21-jre-headless \
        openjdk-21-source \
        python3 \
        python3-setuptools \
        python3-pip \
        gcc \
        python3-dev \
        git \
        gawk \
        make \
        lld \
        bison \
        clang \
        flex \
        libffi-dev \
        libfl-dev \
        libreadline-dev \
        pkg-config \
        tcl-dev \
        zlib1g-dev \
        xdot \
        autoconf \
        help2man \
        && \
    rm -rf /var/lib/apt/lists/*

RUN \
    git clone https://github.com/YosysHQ/yosys.git && \
    cd yosys && \
    git fetch --tags && \
    git checkout v0.60 && \
    git submodule update --init --recursive && \
    make -j$(nproc) && \
    make install && \
    cd .. && \
    rm -rf yosys

RUN \
    git clone https://github.com/verilator/verilator && \
    cd verilator && \
    git fetch --tags && \
    git checkout v5.044 && \
    autoconf && \
    ./configure && \
    make -j$(nproc) && \
    make install && \
    cd .. && \
    rm -rf verilator

WORKDIR /home/${USERNAME}

RUN git clone https://github.com/llvm/circt.git --recursive && \
    cd circt && \
    cmake -G Ninja llvm/llvm -B build \
        -DCMAKE_BUILD_TYPE=Release \
        -DLLVM_ENABLE_ASSERTIONS=ON \
        -DLLVM_TARGETS_TO_BUILD=host \
        -DLLVM_ENABLE_PROJECTS=mlir \
        -DLLVM_EXTERNAL_PROJECTS=circt \
        -DLLVM_EXTERNAL_CIRCT_SOURCE_DIR=$PWD \
        -DLLVM_ENABLE_LLD=ON && \
    ninja -C build bin/firtool && \
    mv ./build ../firtool && \
    cd .. && \
    rm -rf circt

USER ubuntu

WORKDIR /home/ubuntu

# different cases depending on aarch64 or amd64
RUN if [ "$(uname -m)" = "aarch64" ]; then \
    curl -fL "https://github.com/coursier/launchers/raw/master/cs-aarch64-pc-linux.gz" | gzip -d > cs; \
else \
    curl -fL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | gzip -d > cs; \
fi

RUN chmod +x cs && ./cs setup --yes
RUN ./cs install scala:${SCALA_VERSION}
RUN ./cs install sbt
RUN ./cs install scalafmt
RUN rm cs

ENV CHISEL_FIRTOOL_PATH=/home/ubuntu/firtool/bin

ENV PATH="$PATH:/home/ubuntu/firtool/bin"