# Example: Building a Riot application with Docker
# 
# This Dockerfile shows how to use riot-builder to create a production-ready
# application container.

# ==============================================================================
# Build Stage - Use riot-builder to compile your application
# ==============================================================================
FROM ghcr.io/leostera/riot/riot-builder:latest AS build

WORKDIR /app

# Copy your application source
COPY . /app

# Build your application with riot
# Replace 'my-app' with your actual binary name from riot.toml
RUN riot build --release my-app

# The binary will be in _build/release/<arch>/my-app
# List files to verify
RUN ls -la _build/release/*/

# ==============================================================================
# Runtime Stage - Minimal image with just the binary
# ==============================================================================
FROM alpine:latest

# Install minimal runtime dependencies
# Adjust based on your application's needs
RUN apk add --no-cache \
    libgcc \
    libstdc++ \
    ca-certificates

# Copy the built binary from build stage
# Adjust the path to match your architecture
COPY --from=build /app/_build/release/*/my-app /usr/local/bin/my-app

# Create non-root user for security
RUN addgroup -g 1000 riot && \
    adduser -D -u 1000 -G riot riot

# Switch to non-root user
USER riot

# Set the entrypoint
ENTRYPOINT ["/usr/local/bin/my-app"]

# Default command (can be overridden)
CMD ["--help"]
