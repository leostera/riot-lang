# Riot Docker Images

Docker images for building Riot applications with Tusk.

## Quick Start

### Using Pre-built Images (Recommended)

Pull from GitHub Container Registry:

```bash
docker pull ghcr.io/leostera/riot/riot-builder:latest
```

### Use the Builder Image

```dockerfile
# In your application's Dockerfile
FROM ghcr.io/leostera/riot/riot-builder:latest AS build

WORKDIR /app
COPY . /app

# Build your application
RUN tusk build --release my-app

# Multi-stage: Create minimal runtime image
FROM alpine:latest

COPY --from=build /app/_build/release/*/my-app /usr/local/bin/my-app

ENTRYPOINT ["/usr/local/bin/my-app"]
```

### Test Locally

```bash
# Run Tusk directly
docker run --rm -v $(pwd):/app ghcr.io/leostera/riot/riot-builder:latest build --help

# Interactive development
docker run --rm -it -v $(pwd):/app ghcr.io/leostera/riot/riot-builder:latest bash
```

### Build from Source

If you need to build the image locally:

```bash
# From the root of the Riot repository
./docker/build.sh
```

### Manual Publish

Until the GitHub Actions Docker publishing workflow is re-enabled, publish the
builder image manually:

```bash
./docker/publish.sh
```

This builds `riot-builder:local`, smoke-tests it, tags:

- `ghcr.io/leostera/riot/riot-builder:latest`
- `ghcr.io/leostera/riot/riot-builder:sha-<git-short-sha>`

and pushes both tags.

To inspect the commands without pushing:

```bash
./docker/publish.sh --no-push --dry-run
```

## Image Architecture

```
┌─────────────────────────────────────┐
│ Stage 1: Bootstrap                  │
│ - Ubuntu 24.04                      │
│ - Run ./bootstrap.py                │
│ - Run ./minitusk                    │
│ - Build tusk with bootstrap tusk    │
│ - Result: _build/debug/*/tusk       │
└────────────┬────────────────────────┘
             │ COPY tusk binary
             ↓
┌─────────────────────────────────────┐
│ Stage 2: Builder (Final Image)     │
│ - Clean Ubuntu 24.04                │
│ - Tusk in /usr/local/bin/           │
│ - Ready for building apps           │
└─────────────────────────────────────┘
```

## Image Details

- **Base OS:** Ubuntu 24.04 LTS
- **Tusk Location:** `/usr/local/bin/tusk`
- **Working Directory:** `/app`
- **Entrypoint:** `tusk`
- **Cross-Compilation Targets:**
  - `aarch64-linux-gnu` (ARM64 glibc)
  - `x86_64-linux-gnu` (x86-64 glibc)

## Building Applications

### Simple Application

```bash
docker run --rm -v $(pwd):/app ghcr.io/leostera/riot/riot-builder:latest build my-app
```

### With Release Profile

```bash
docker run --rm -v $(pwd):/app ghcr.io/leostera/riot/riot-builder:latest build --release my-app
```

### Cross-Compilation

Build for different architectures:

```bash
# Build for ARM64 Linux
docker run --rm -v $(pwd):/app ghcr.io/leostera/riot/riot-builder:latest build -x aarch64-linux-gnu my-app

# Build for x86-64 Linux
docker run --rm -v $(pwd):/app ghcr.io/leostera/riot/riot-builder:latest build -x x86_64-linux-gnu my-app
```

## Development Workflow

### Using docker-compose

Create `docker-compose.yml` in your project:

```yaml
version: '3.8'

services:
  build:
    image: ghcr.io/leostera/riot/riot-builder:latest
    volumes:
      - .:/app
      - tusk-cache:/root/.tusk
    working_dir: /app
    command: build --watch

volumes:
  tusk-cache:
```

Then:

```bash
docker-compose up
```

## Troubleshooting

### Build Fails in Bootstrap Stage

Check that all required files are present:
- `bootstrap.py`
- `minitusk` (will be created by bootstrap.py)
- `packages/` directory with all Tusk source code

### Tusk Not Found

Verify the binary was copied correctly:

```bash
docker run --rm riot-builder:latest which tusk
docker run --rm riot-builder:latest tusk --help
```

### Permission Issues

If you get permission errors when mounting volumes:

```bash
docker run --rm -v $(pwd):/app -u $(id -u):$(id -g) riot-builder:latest build
```

## Published Images

The practical published image refs for the current manual flow are:

- **Latest stable:** `ghcr.io/leostera/riot/riot-builder:latest`
- **Specific commit:** `ghcr.io/leostera/riot/riot-builder:sha-<commit>`

At the time of writing, the repository's Docker publishing workflow is
disabled, so these tags are only as current as the last manual publish.

### Image Tags

- `latest` - Latest build from main branch (recommended for most users)
- `sha-xxxxxxx` - Specific commit (for reproducible builds)
- `main` - Latest main branch build

## Future Enhancements

- [x] Add cross-compilation toolchains (aarch64, x86_64)
- [x] Pre-install OCaml toolchain (no download needed on first run)
- [x] Publish to GitHub Container Registry (ghcr.io)
- [ ] Add musl-based cross-compilers for static binaries
- [ ] Multi-architecture support (linux/amd64, linux/arm64)
- [ ] Add example applications
- [ ] Create docker-compose templates

## CI/CD Integration

### GitHub Actions Example

Use the pre-built image for faster CI/CD:

```yaml
name: Build with Docker

on: [push]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Build application
        run: |
          docker run --rm -v $(pwd):/app \
            ghcr.io/leostera/riot/riot-builder:latest \
            build my-app
      
      - name: Upload artifact
        uses: actions/upload-artifact@v4
        with:
          name: my-app
          path: _build/debug/*/my-app
```

### Using in docker-compose

```yaml
version: '3.8'

services:
  build:
    image: ghcr.io/leostera/riot/riot-builder:latest
    volumes:
      - .:/app
      - tusk-cache:/root/.tusk
    working_dir: /app
    command: build --watch

volumes:
  tusk-cache:
```

## License

Same as Riot/Tusk - see main repository LICENSE file.
