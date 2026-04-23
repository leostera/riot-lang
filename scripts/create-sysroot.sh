#!/bin/bash
# Create a Linux sysroot overlay for cross-compilation.
# This script uses Docker to extract a target-specific Ubuntu userspace and
# copy its multiarch runtime/devel layout into the GCC sysroot shape used by
# Riot's bundled Linux toolchains. The files are also mirrored into the flat
# lib/usr/lib paths expected by parts of the current toolchain packaging. That
# keeps the glibc baseline anchored to the selected Ubuntu release instead of
# whatever the host cross toolchain shipped.

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
mkdir -p \
  "$SYSROOT_DIR/lib" \
  "$SYSROOT_DIR/lib/$LIB_DIR" \
  "$SYSROOT_DIR/usr/include" \
  "$SYSROOT_DIR/usr/lib" \
  "$SYSROOT_DIR/usr/lib/$LIB_DIR"

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
  libpcre2-dev \
  zlib1g-dev

# Extract headers
echo "Extracting headers..."
docker exec "$CONTAINER" tar czf /tmp/riot-sysroot-headers.tar -C /usr/include .
docker cp "$CONTAINER:/tmp/riot-sysroot-headers.tar" "$OUTPUT_ROOT/riot-sysroot-headers-${TARGET}.tar.gz"
tar xzf "$OUTPUT_ROOT/riot-sysroot-headers-${TARGET}.tar.gz" -C "$SYSROOT_DIR/usr/include"
rm -f "$OUTPUT_ROOT/riot-sysroot-headers-${TARGET}.tar.gz"
cp -a "$SYSROOT_DIR/usr/include/${LIB_DIR}/." "$SYSROOT_DIR/usr/include/"
docker cp "$CONTAINER:/usr/include/${LIB_DIR}/openssl/." "$SYSROOT_DIR/usr/include/openssl/" 2>/dev/null || true

# Extract the distro runtime + linker inputs. Keep the native multiarch layout
# because Ubuntu's libc.so linker script references absolute multiarch paths
# under --sysroot, and also mirror files into flat lib/usr/lib for existing
# Riot toolchain packaging expectations.
echo "Extracting glibc runtime and development libraries..."
docker exec "$CONTAINER" bash -lc "
  set -euo pipefail
  rm -rf /tmp/riot-sysroot-glibc
  mkdir -p \
    /tmp/riot-sysroot-glibc/lib/${LIB_DIR} \
    /tmp/riot-sysroot-glibc/usr/lib/${LIB_DIR}
  cp -a /lib/${LIB_DIR}/. /tmp/riot-sysroot-glibc/lib/
  cp -a /lib/${LIB_DIR}/. /tmp/riot-sysroot-glibc/lib/${LIB_DIR}/
  cp -a /usr/lib/${LIB_DIR}/. /tmp/riot-sysroot-glibc/usr/lib/
  cp -a /usr/lib/${LIB_DIR}/. /tmp/riot-sysroot-glibc/usr/lib/${LIB_DIR}/
  find /tmp/riot-sysroot-glibc/lib -type f -name '*.a' -delete
  tar czf /tmp/riot-sysroot-glibc.tar -C /tmp/riot-sysroot-glibc .
"
docker cp "$CONTAINER:/tmp/riot-sysroot-glibc.tar" "$OUTPUT_ROOT/riot-sysroot-glibc-${TARGET}.tar.gz"
tar xzf "$OUTPUT_ROOT/riot-sysroot-glibc-${TARGET}.tar.gz" -C "$SYSROOT_DIR"
rm -f "$OUTPUT_ROOT/riot-sysroot-glibc-${TARGET}.tar.gz"

echo "Rewriting absolute sysroot symlinks..."
find "$SYSROOT_DIR" -type l -print | while IFS= read -r link_path; do
  link_target="$(readlink "$link_path")"
  case "$link_target" in
    /*)
      sysroot_target="$SYSROOT_DIR$link_target"
      [ -e "$sysroot_target" ] || continue
      link_dir="$(dirname "$link_path")"
      relative_target="$(python3 -c 'import os, sys; print(os.path.relpath(sys.argv[1], sys.argv[2]))' "$sysroot_target" "$link_dir")"
      ln -snf "$relative_target" "$link_path"
      ;;
  esac
done

# The Homebrew-built cross GCCs search lib64 for libgcc_s on both Linux
# targets. Ubuntu ships libgcc_s.so.1 but not an unversioned linker input in
# the base runtime, so provide the expected development symlink in each mirrored
# library directory.
for libgcc_dir in \
  "$SYSROOT_DIR/lib" \
  "$SYSROOT_DIR/lib/$LIB_DIR" \
  "$SYSROOT_DIR/usr/lib" \
  "$SYSROOT_DIR/usr/lib/$LIB_DIR"
do
  if [ -e "$libgcc_dir/libgcc_s.so.1" ] && [ ! -e "$libgcc_dir/libgcc_s.so" ]; then
    (cd "$libgcc_dir" && ln -s libgcc_s.so.1 libgcc_s.so)
  fi
done

(cd "$SYSROOT_DIR" && ln -sf lib lib64)
(cd "$SYSROOT_DIR/usr" && ln -sf lib lib64)

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
