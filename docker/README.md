# Riot Docker Images

Docker images for building Riot applications with Riot.

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
RUN riot build --release my-app

# Multi-stage: Create minimal runtime image
FROM alpine:latest

COPY --from=build /app/_build/release/*/my-app /usr/local/bin/my-app

ENTRYPOINT ["/usr/local/bin/my-app"]
```

### Test Locally

```bash
# Run Riot directly
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
│ - Run ./miniriot                    │
│ - Build riot with bootstrap riot    │
│ - Result: _build/debug/*/riot       │
└────────────┬────────────────────────┘
             │ COPY riot binary
             ↓
┌─────────────────────────────────────┐
│ Stage 2: Builder (Final Image)     │
│ - Clean Ubuntu 24.04                │
│ - Riot in /usr/local/bin/           │
│ - Ready for building apps           │
└─────────────────────────────────────┘
```

## Image Details

- **Base OS:** Ubuntu 24.04 LTS
- **Riot Location:** `/usr/local/bin/riot`
- **Working Directory:** `/app`
- **Entrypoint:** `riot`
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
      - riot-cache:/root/.riot
    working_dir: /app
    command: build --watch

volumes:
  riot-cache:
```

Then:

```bash
docker-compose up
```

## Troubleshooting

### Build Fails in Bootstrap Stage

Check that all required files are present:
- `bootstrap.py`
- `miniriot` (will be created by bootstrap.py)
- `packages/` directory with all Riot source code

### Riot Not Found

Verify the binary was copied correctly:

```bash
docker run --rm riot-builder:latest which riot
docker run --rm riot-builder:latest riot --help
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

Use the local `setup-riot` action to install Riot on the runner for the rest of
the job:

```yaml
name: Build with Riot

on: [push]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - uses: ./docker/setup-riot

      - run: riot build my-app
      - run: riot test my-app_tests
```

The action runs the standard installer script, then adds `~/.riot/bin` to the
job `PATH`. See `docker/setup-riot/README.md` for inputs and outputs.

### Using in docker-compose

```yaml
version: '3.8'

services:
  build:
    image: ghcr.io/leostera/riot/riot-builder:latest
    volumes:
      - .:/app
      - riot-cache:/root/.riot
    working_dir: /app
    command: build --watch

volumes:
  riot-cache:
```

## License

Same as Riot/Riot - see main repository LICENSE file.
