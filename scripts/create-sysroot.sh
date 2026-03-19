#!/bin/bash
# Create minimal sysroot for Linux cross-compilation
# This script uses Docker to extract system libraries and headers from Ubuntu

set -e

TARGET="${1:-aarch64-unknown-linux-gnu}"
UBUNTU_VERSION="${2:-22.04}"

case "$TARGET" in
  aarch64-unknown-linux-gnu)
    ARCH="arm64"
    DOCKER_PLATFORM="linux/arm64"
    LIB_DIR="aarch64-linux-gnu"
    ;;
  x86_64-unknown-linux-gnu)
    ARCH="amd64"
    DOCKER_PLATFORM="linux/amd64"
    LIB_DIR="x86_64-linux-gnu"
    ;;
  *)
    echo "Unsupported target: $TARGET"
    echo "Supported: aarch64-unknown-linux-gnu, x86_64-unknown-linux-gnu"
    exit 1
    ;;
esac

SYSROOT_DIR="sysroot-${TARGET}"
DOCKER_IMAGE="ubuntu:${UBUNTU_VERSION}"

echo "Creating sysroot for $TARGET using $DOCKER_IMAGE..."

# Clean previous sysroot
rm -rf "$SYSROOT_DIR"
mkdir -p "$SYSROOT_DIR/usr"/{include,lib}

# Start container
echo "Starting Docker container..."
CONTAINER=$(docker run --platform="$DOCKER_PLATFORM" -d "$DOCKER_IMAGE" sleep 3600)

cleanup() {
  echo "Cleaning up container..."
  docker stop "$CONTAINER" >/dev/null 2>&1 || true
  docker rm "$CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT

# Install development packages
echo "Installing development packages..."
docker exec "$CONTAINER" apt-get update -qq
docker exec "$CONTAINER" apt-get install -y -qq \
  libc6-dev \
  linux-libc-dev \
  uuid-dev \
  libssl-dev \
  zlib1g-dev

# Extract headers
echo "Extracting headers..."
docker cp "$CONTAINER:/usr/include/." "$SYSROOT_DIR/usr/include/"

# Extract libraries
echo "Extracting libraries..."
docker cp "$CONTAINER:/usr/lib/${LIB_DIR}/." "$SYSROOT_DIR/usr/lib/" 2>/dev/null || true
docker cp "$CONTAINER:/lib/${LIB_DIR}/." "$SYSROOT_DIR/usr/lib/" 2>/dev/null || true

# Create lib64 symlink if needed (x86_64)
if [ "$ARCH" = "amd64" ]; then
  (cd "$SYSROOT_DIR/usr" && ln -sf lib lib64)
fi

# Create tarball
TARBALL="${SYSROOT_DIR}.tar.gz"
echo "Creating tarball: $TARBALL"
tar czf "$TARBALL" "$SYSROOT_DIR"

# Show size
SIZE=$(du -h "$TARBALL" | cut -f1)
echo ""
echo "✅ Sysroot created successfully!"
echo "   Target:   $TARGET"
echo "   Path:     $SYSROOT_DIR"
echo "   Tarball:  $TARBALL ($SIZE)"
echo ""
echo "To use this sysroot:"
echo "  1. Extract to toolchain directory:"
echo "     tar xzf $TARBALL -C ~/.tusk/toolchains/${TARGET}/"
echo ""
echo "  2. Configure compiler to use it:"
echo "     export SYSROOT=~/.tusk/toolchains/${TARGET}/sysroot"
echo "     ${TARGET}-gcc --sysroot=\$SYSROOT ..."
echo ""

# List key libraries
echo "Included libraries:"
find "$SYSROOT_DIR/usr/lib" -maxdepth 1 -name "*.so" -o -name "*.a" | sort | head -20
