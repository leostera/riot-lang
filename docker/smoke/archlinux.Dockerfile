# syntax=docker/dockerfile:1.7

ARG ARCH_BASE_IMAGE=archlinux:latest
FROM --platform=$TARGETPLATFORM ${ARCH_BASE_IMAGE}

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN pacman -Sy --noconfirm --disable-sandbox base-devel ca-certificates \
  && pacman -Scc --noconfirm

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
