# Example: Using riot-builder Docker Image

This example shows how to use the `riot-builder` Docker image to build and package your Riot application.

## Prerequisites

- Docker installed
- A Riot application with a `riot.toml` file

## Quick Start

### 1. Copy the Example Dockerfile

Copy `Dockerfile` from this directory to your application root:

```bash
cp docker/example-app/Dockerfile /path/to/your/app/
```

### 2. Customize for Your App

Edit the Dockerfile and replace `my-app` with your actual binary name:

```dockerfile
RUN riot build --release my-app
```

The binary name comes from your `riot.toml` `[[bin]]` section:

```toml
[[bin]]
name = "my-app"  # <-- Use this name
path = "src/main.ml"
```

### 3. Build Your Docker Image

```bash
docker build -t my-app:latest .
```

### 4. Run Your Application

```bash
docker run --rm my-app:latest
```

## Multi-Stage Build Benefits

The example Dockerfile uses multi-stage builds:

- **Build Stage**: Uses `riot-builder` (~2GB) to compile your app
- **Runtime Stage**: Uses Alpine Linux (~10-20MB) with just your binary

Result: Fast builds, tiny production images!

## Local Development

For development, you can use the builder image directly:

```bash
# Build your app
docker run --rm -v $(pwd):/app ghcr.io/leostera/riot/riot-builder:latest build my-app

# Interactive shell
docker run --rm -it -v $(pwd):/app ghcr.io/leostera/riot/riot-builder:latest bash

# Watch mode (if supported)
docker run --rm -v $(pwd):/app ghcr.io/leostera/riot/riot-builder:latest build --watch
```

## Advanced: Cross-Compilation

When cross-compilation support is added, you'll be able to build for different architectures:

```dockerfile
# Build for ARM64 Alpine (musl)
RUN riot build --release -x aarch64-unknown-linux-musl my-app

# Build for x86_64 Alpine (musl)
RUN riot build --release -x x86_64-unknown-linux-musl my-app

# Build for ARM64 Ubuntu (glibc)
RUN riot build --release -x aarch64-unknown-linux-gnu my-app
```

## Customizing Runtime Dependencies

If your app needs additional libraries at runtime, add them to the Alpine stage:

```dockerfile
FROM alpine:latest

# Add runtime dependencies
RUN apk add --no-cache \
    libgcc \
    libstdc++ \
    ca-certificates \
    sqlite-libs \      # For database apps
    libssl3 \          # For TLS/HTTPS
    libcrypto3         # For crypto operations
```

## Using Docker Compose

Create `docker-compose.yml`:

```yaml
version: '3.8'

services:
  app:
    build: .
    ports:
      - "8080:8080"
    environment:
      - APP_ENV=production
    restart: unless-stopped
```

Then:

```bash
docker-compose up -d
```

## CI/CD Integration

### GitHub Actions

```yaml
name: Build and Deploy

on: [push]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Build Docker image
        run: docker build -t my-app:${{ github.sha }} .
      
      - name: Push to registry
        run: |
          echo "${{ secrets.GITHUB_TOKEN }}" | docker login ghcr.io -u ${{ github.actor }} --password-stdin
          docker tag my-app:${{ github.sha }} ghcr.io/${{ github.repository }}:latest
          docker push ghcr.io/${{ github.repository }}:latest
```

## Troubleshooting

### Binary not found after build

Check the exact path where riot places the binary:

```bash
docker run --rm -v $(pwd):/app ghcr.io/leostera/riot/riot-builder:latest sh -c "riot build my-app && find _build -name my-app"
```

### Permission denied

Make sure the binary is executable:

```dockerfile
COPY --from=build /app/_build/release/*/my-app /usr/local/bin/my-app
RUN chmod +x /usr/local/bin/my-app
```

### Image too large

Use Alpine as the runtime base (not Ubuntu/Debian):

```dockerfile
FROM alpine:latest  # ✓ ~5MB base
# Not: FROM ubuntu:latest  # ✗ ~77MB base
```

## Next Steps

- See `docker/README.md` for more details about the riot-builder image
- Check out more examples in the Riot repository
- Join the Riot community for help and discussions
