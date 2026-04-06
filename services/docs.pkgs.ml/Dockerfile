FROM docker.io/cloudflare/sandbox:0.8.4 AS sandbox-base

FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
ENV HOME=/root
ENV PATH=/root/.riot/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    curl \
    git \
    libpcre2-dev \
    libssl-dev \
    libzstd-dev \
    pkg-config \
    tar \
    uuid-dev \
    xz-utils \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

COPY --from=sandbox-base /container-server/sandbox /sandbox

RUN curl -sSL https://get.riot.ml | sh - \
    && /root/.riot/bin/riot version

RUN mkdir -p /workspace

WORKDIR /workspace

EXPOSE 3000

ENTRYPOINT ["/sandbox"]
