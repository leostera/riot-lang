# Docker Quick Start Guide

Get started with Riot Docker images in 5 minutes!

## 🚀 For Users: Use Pre-built Images

### Pull the Image

```bash
docker pull ghcr.io/leostera/riot/riot-builder:latest
```

### Build Your App

```bash
# In your Riot project directory
docker run --rm -v $(pwd):/app ghcr.io/leostera/riot/riot-builder:latest build my-app
```

### Package for Production

Create `Dockerfile` in your project:

```dockerfile
FROM ghcr.io/leostera/riot/riot-builder:latest AS build
WORKDIR /app
COPY . /app
RUN riot build --release my-app

FROM alpine:latest
COPY --from=build /app/_build/release/*/my-app /usr/local/bin/my-app
ENTRYPOINT ["/usr/local/bin/my-app"]
```

Then build:

```bash
docker build -t my-app:latest .
docker run --rm my-app:latest
```

## 🛠️ For Developers: Build the Image Locally

### Prerequisites

- Docker installed
- Git checkout of Riot repository

### Build riot-builder

```bash
cd /path/to/riot
./docker/build.sh
```

This will:
1. Bootstrap Riot from source
2. Create a clean builder image with Riot installed
3. Tag it as `riot-builder:latest`

### Publish riot-builder

If you want to refresh the shared GHCR image manually:

```bash
./docker/publish.sh
```

To preview the commands without pushing:

```bash
./docker/publish.sh --no-push --dry-run
```

### Verify

```bash
docker run --rm riot-builder:latest --help
```

### Test with the Riot Codebase

```bash
# Build riot-cli using the Docker image
docker run --rm -v $(pwd):/app riot-builder:latest build riot-cli

# Check the output
ls -la _build/debug/*/riot
```

## 📂 Files

```
docker/
├── Dockerfile              # Main multi-stage Dockerfile
├── build.sh               # Helper script to build images
├── README.md              # Full documentation
├── QUICKSTART.md          # This file
└── example-app/
    ├── Dockerfile         # Example app template
    └── README.md          # Detailed example docs
```

## 🎯 Next Steps

- **Users**: See `example-app/README.md` for complete examples
- **Developers**: See `README.md` for architecture details
- **CI/CD**: See `docker/setup-riot/README.md` for the installer-based GitHub Actions setup helper and `.github/workflows/docker-build.yml` for the disabled image-publish workflow snapshot

## 🐛 Troubleshooting

### Image build fails at bootstrap stage

Make sure you're in the Riot repository root:

```bash
cd /path/to/riot
ls bootstrap.py  # Should exist
./docker/build.sh
```

### Can't pull from ghcr.io

The Docker publishing workflow is currently disabled. If you need a fresh image,
build or publish it manually from this repository.

### Binary not found after build

Check where riot places the binary:

```bash
docker run --rm -v $(pwd):/app riot-builder:latest sh -c "riot build my-app && find _build -name 'my-app'"
```

## 📝 Notes

- The builder image is ~2GB (includes full OCaml toolchain)
- Runtime images can be ~10-20MB (Alpine + binary only)
- Multi-stage builds keep production images small
- All images use Ubuntu 24.04 LTS as base

## 🤝 Contributing

Found an issue or want to improve the Docker setup? PRs welcome!

1. Test locally with `./docker/build.sh`
2. Verify the image works with example apps
3. Update documentation
4. Submit PR
