# syntax=docker/dockerfile:1.7

FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
WORKDIR /work

RUN apt-get update && apt-get install -y \
    bash \
    build-essential \
    ca-certificates \
    curl \
    file \
    g++-mingw-w64-x86-64 \
    gcc-mingw-w64-x86-64 \
    gawk \
    git \
    libpcre2-dev \
    libssl-dev \
    libzstd-dev \
    m4 \
    musl-tools \
    pkg-config \
    rsync \
    uuid-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

COPY docker/ocaml-toolchain-run.sh /usr/local/bin/ocaml-toolchain-run

RUN chmod +x /usr/local/bin/ocaml-toolchain-run

ENTRYPOINT ["/usr/local/bin/ocaml-toolchain-run"]
