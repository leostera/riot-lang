# syntax=docker/dockerfile:1.7

FROM --platform=$TARGETPLATFORM ubuntu:24.04

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    curl \
    gzip \
    libpcre2-dev \
    libssl-dev \
    tar \
    uuid-dev \
    zlib1g-dev \
  && rm -rf /var/lib/apt/lists/*

RUN --mount=from=riot-bin,source=riot,target=/tmp/riot,readonly \
  install -m 0755 /tmp/riot /usr/local/bin/riot \
  && riot --version

WORKDIR /workspace

RUN riot init hello-world --bin

WORKDIR /workspace/hello-world

RUN riot build

RUN riot run | tee /tmp/riot-run.out \
  && grep -F "Hello from hello-world" /tmp/riot-run.out

RUN riot test --small
