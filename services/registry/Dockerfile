# Build with:
#   docker build -f services/registry/Dockerfile .

FROM ghcr.io/leostera/riot/riot-builder:latest AS build

WORKDIR /app

COPY . /app

RUN tusk build --release registry

FROM ubuntu:24.04 AS runtime

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

RUN useradd --create-home --uid 10001 riot

WORKDIR /app

COPY --from=build /app/_build/release/*/out/registry/registry /usr/local/bin/registry

USER riot

ENTRYPOINT ["/usr/local/bin/registry"]
