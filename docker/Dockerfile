# Multi-stage Dockerfile for Riot/Riot
# Stage 1: Bootstrap Riot from source
# Stage 2: Clean builder image with Riot installed

# ==============================================================================
# Stage 1: Bootstrap Riot
# ==============================================================================
FROM ubuntu:24.04 AS bootstrap

LABEL org.opencontainers.image.source="https://github.com/leostera/riot"
LABEL org.opencontainers.image.description="Riot/Riot bootstrap stage"

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

# Set working directory for Riot source
WORKDIR /riot

# Copy Riot source code
COPY . /riot

# Step 1: Bootstrap with Python
RUN echo "=== Step 1: Running bootstrap.py ===" && \
    ./bootstrap.py

# Step 2: Build miniriot
RUN echo "=== Step 2: Running miniriot ===" && \
    ./miniriot

# Step 3: Build Riot proper using the bootstrap Riot
RUN echo "=== Step 3: Building Riot proper ===" && \
    ./_build/bootstrap/out/Riot_cli/riot -v build riot-cli && \
    ./_build/bootstrap/out/Riot_cli/riot -v install riot


# Verify the final Riot binary exists and works
RUN echo "=== Verifying Riot binary ===" && \
    ls -la ./riot && ./riot --help

# ==============================================================================
# Stage 2: Clean Builder Image
# ==============================================================================
FROM ubuntu:24.04 AS builder

LABEL org.opencontainers.image.source="https://github.com/leostera/riot"
LABEL org.opencontainers.image.description="Riot/Riot builder image"

# Install runtime dependencies for Riot and cross-compilation toolchains
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

# Copy the fully built Riot binary from bootstrap stage
COPY --from=bootstrap /riot/riot /usr/local/bin/riot

# Copy the OCaml toolchain from bootstrap stage
# This avoids downloading it every time the container runs
COPY --from=bootstrap /root/.riot/toolchains /root/.riot/toolchains

# Make it executable
RUN chmod +x /usr/local/bin/riot

# Verify Riot installation
RUN riot --version
RUN riot --help

# Set working directory for user applications
WORKDIR /app

# Default command
ENTRYPOINT ["riot"]
CMD ["--help"]
