# Riot Docker Images

Docker images for building Riot applications with Tusk.

## Quick Start

### Build the Builder Image

```bash
# From the root of the Riot repository
docker build -t riot-builder:latest -f docker/Dockerfile .
```

### Use the Builder Image

```dockerfile
# In your application's Dockerfile
FROM riot-builder:latest AS build

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
docker run --rm -v $(pwd):/app riot-builder:latest build --help

# Interactive development
docker run --rm -it -v $(pwd):/app riot-builder:latest bash
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
docker run --rm -v $(pwd):/app riot-builder:latest build my-app
```

### With Release Profile

```bash
docker run --rm -v $(pwd):/app riot-builder:latest build --release my-app
```

### Cross-Compilation

Build for different architectures:

```bash
# Build for ARM64 Linux
docker run --rm -v $(pwd):/app riot-builder:latest build -x aarch64-linux-gnu my-app

# Build for x86-64 Linux
docker run --rm -v $(pwd):/app riot-builder:latest build -x x86_64-linux-gnu my-app
```

## Development Workflow

### Using docker-compose

Create `docker-compose.yml` in your project:

```yaml
version: '3.8'

services:
  build:
    image: riot-builder:latest
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

## Future Enhancements

- [x] Add cross-compilation toolchains (aarch64, x86_64)
- [ ] Add musl-based cross-compilers for static binaries
- [ ] Pre-install common OCaml packages
- [ ] Multi-architecture support (linux/amd64, linux/arm64)
- [ ] Publish to GitHub Container Registry (ghcr.io)
- [ ] Add example applications
- [ ] Create docker-compose templates

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Build with Docker

on: [push]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Build Riot builder image
        run: docker build -t riot-builder:latest -f docker/Dockerfile .
      
      - name: Build application
        run: docker run --rm -v $(pwd):/app riot-builder:latest build my-app
      
      - name: Upload artifact
        uses: actions/upload-artifact@v4
        with:
          name: my-app
          path: _build/debug/*/my-app
```

## License

Same as Riot/Tusk - see main repository LICENSE file.
