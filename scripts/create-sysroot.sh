#!/bin/bash
# Create a minimal Linux SDK overlay for cross-compilation.
# This script uses Docker to extract only the non-libc headers and libraries
# that Riot's foreign stubs need on Linux.

set -e

TARGET="${1:-aarch64-unknown-linux-gnu}"
UBUNTU_VERSION="${2:-22.04}"
OUTPUT_ROOT="${3:-$PWD}"

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

SYSROOT_DIR="${OUTPUT_ROOT%/}/sysroot-${TARGET}"
DOCKER_IMAGE="ubuntu:${UBUNTU_VERSION}"

echo "Creating sysroot for $TARGET using $DOCKER_IMAGE..."

# Clean previous sysroot
mkdir -p "$OUTPUT_ROOT"
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
docker cp "$CONTAINER:/usr/include/uuid" "$SYSROOT_DIR/usr/include/"
docker cp "$CONTAINER:/usr/include/openssl" "$SYSROOT_DIR/usr/include/"
docker cp "$CONTAINER:/usr/include/zlib.h" "$SYSROOT_DIR/usr/include/"
docker cp "$CONTAINER:/usr/include/zconf.h" "$SYSROOT_DIR/usr/include/"
docker cp "$CONTAINER:/usr/include/${LIB_DIR}/openssl/." "$SYSROOT_DIR/usr/include/openssl/" 2>/dev/null || true

# Extract libraries without overwriting the bundled glibc sysroot.
echo "Extracting libraries..."
docker exec "$CONTAINER" bash -lc "
  set -euo pipefail
  shopt -s nullglob
  rm -rf /tmp/riot-sdk-libs
  mkdir -p /tmp/riot-sdk-libs
  for pattern in libuuid.so* libuuid.a libssl.so* libssl.a libcrypto.so* libcrypto.a libz.so* libz.a; do
    for file in /usr/lib/${LIB_DIR}/\$pattern /lib/${LIB_DIR}/\$pattern; do
      [ -e \"\$file\" ] || continue
      cp -a \"\$file\" /tmp/riot-sdk-libs/
    done
  done
  tar czf /tmp/riot-sdk-libs.tar -C /tmp/riot-sdk-libs .
"
docker cp "$CONTAINER:/tmp/riot-sdk-libs.tar" "$OUTPUT_ROOT/riot-sdk-libs-${TARGET}.tar.gz"
tar xzf "$OUTPUT_ROOT/riot-sdk-libs-${TARGET}.tar.gz" -C "$SYSROOT_DIR/usr/lib/"
rm -f "$OUTPUT_ROOT/riot-sdk-libs-${TARGET}.tar.gz"

# Create lib64 symlink if needed (x86_64)
if [ "$ARCH" = "amd64" ]; then
  (cd "$SYSROOT_DIR/usr" && ln -sf lib lib64)
fi

# Create tarball
TARBALL="${OUTPUT_ROOT%/}/sysroot-${TARGET}.tar.gz"
echo "Creating tarball: $TARBALL"
tar czf "$TARBALL" "$SYSROOT_DIR"

# Show size
SIZE=$(du -h "$TARBALL" | cut -f1)
echo ""
echo "✅ SDK overlay created successfully!"
echo "   Target:   $TARGET"
echo "   Path:     $SYSROOT_DIR"
echo "   Tarball:  $TARBALL ($SIZE)"
echo ""
echo "To use this sysroot:"
echo "  1. Extract to toolchain directory:"
echo "     tar xzf $TARBALL -C ~/.riot/toolchains/${TARGET}/"
echo ""
echo "  2. Configure compiler to use it:"
echo "     export SYSROOT=~/.riot/toolchains/${TARGET}/sysroot"
echo "     ${TARGET}-gcc --sysroot=\$SYSROOT ..."
echo ""

# List key libraries
echo "Included libraries:"
find "$SYSROOT_DIR/usr/lib" -maxdepth 1 \( -name "*.so" -o -name "*.a" \) | sort
