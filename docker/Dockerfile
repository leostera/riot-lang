# Multi-stage Dockerfile for Riot/Tusk
# Stage 1: Bootstrap Tusk from source
# Stage 2: Clean builder image with Tusk installed

# ==============================================================================
# Stage 1: Bootstrap Tusk
# ==============================================================================
FROM ubuntu:24.04 AS bootstrap

# Build arg to bust cache when needed
ARG CACHEBUST=1

LABEL org.opencontainers.image.source="https://github.com/leostera/riot"
LABEL org.opencontainers.image.description="Riot/Tusk bootstrap stage"

# Install system dependencies for bootstrapping
RUN apt-get update && apt-get install -y \
    curl \
    git \
    build-essential \
    python3 \
    python3-pip \
    # Native dependencies for kernel package
    libssl-dev \
    uuid-dev \
    libzstd-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory for Tusk source
WORKDIR /riot

# Copy Tusk source code
COPY . /riot

# Step 1: Bootstrap with Python
RUN echo "=== Step 1: Running bootstrap.py ===" && \
    ./bootstrap.py

# Step 2: Build minitusk
RUN echo "=== Step 2: Running minitusk ===" && \
    ./minitusk

# Step 3: Build Tusk proper using the bootstrap Tusk
RUN echo "=== Step 3: Building Tusk proper ===" && \
    ./_build/bootstrap/out/Tusk_cli/tusk build --no-code-server tusk-cli && \
    ./_build/bootstrap/out/Tusk_cli/tusk install tusk


# Verify the final Tusk binary exists and works
RUN echo "=== Verifying Tusk binary ===" && \
    ls -la ./tusk && ./tusk --help

# ==============================================================================
# Stage 2: Clean Builder Image
# ==============================================================================
FROM ubuntu:24.04 AS builder

LABEL org.opencontainers.image.source="https://github.com/leostera/riot"
LABEL org.opencontainers.image.description="Riot/Tusk builder image"

# Install runtime dependencies for Tusk and cross-compilation toolchains
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    # Native dependencies for kernel package
    libssl-dev \
    uuid-dev \
    libzstd-dev \
    # Cross-compilation toolchains
    gcc-aarch64-linux-gnu \
    g++-aarch64-linux-gnu \
    gcc-x86-64-linux-gnu \
    g++-x86-64-linux-gnu \
    # Additional cross-compilation support libraries
    libc6-dev-arm64-cross \
    libc6-dev-amd64-cross \
    && rm -rf /var/lib/apt/lists/*

# Copy the fully built Tusk binary from bootstrap stage
COPY --from=bootstrap /riot/tusk /usr/local/bin/tusk

# Copy the OCaml toolchain from bootstrap stage
# This avoids downloading it every time the container runs
COPY --from=bootstrap /root/.tusk/toolchains /root/.tusk/toolchains

# Make it executable
RUN chmod +x /usr/local/bin/tusk

# Verify Tusk installation
RUN tusk --version
RUN tusk --help

# Set working directory for user applications
WORKDIR /app

# Default command
ENTRYPOINT ["tusk"]
CMD ["--help"]
