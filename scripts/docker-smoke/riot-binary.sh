#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

DISTRO="${DISTRO:-archlinux}"
PLATFORM="${PLATFORM:-linux/arm64}"
TARGET_TRIPLE="${TARGET_TRIPLE:-}"
RIOT_BIN="${RIOT_BIN:-}"
BUILD_RIOT=1
NO_CACHE=0
PROGRESS="${PROGRESS:-plain}"
ARCH_BASE_IMAGE="${ARCH_BASE_IMAGE:-}"

usage() {
  cat <<'USAGE'
Usage: scripts/docker-smoke/riot-binary.sh [OPTIONS]

Build or reuse a local Riot binary, mount it into a Linux container during
docker build, and smoke-test generated workspaces with riot init/build/run/test.

Options:
  --distro DISTRO       archlinux or ubuntu (default: archlinux)
  --platform PLATFORM   Docker platform, linux/arm64 or linux/amd64 (default: linux/arm64)
  --target TRIPLE       Riot target triple (derived from --platform by default)
  --riot-bin PATH       Existing Riot binary to mount (default: _build/debug/<target>/out/riot-cli/riot)
  --no-build            Do not build riot-cli before running the Docker smoke
  --no-cache            Pass --no-cache to docker buildx build
  --progress MODE       Docker build progress mode (default: plain)
  --arch-base IMAGE     Override the Arch base image
  -h, --help            Show this help

Examples:
  scripts/docker-smoke/riot-binary.sh
  scripts/docker-smoke/riot-binary.sh --distro archlinux --platform linux/arm64
  scripts/docker-smoke/riot-binary.sh --distro ubuntu --platform linux/amd64
USAGE
}

die() {
  printf 'error: %s\n' "$*" >&2
  exit 1
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --distro)
      [ "$#" -ge 2 ] || die "--distro requires a value"
      DISTRO="$2"
      shift 2
      ;;
    --platform)
      [ "$#" -ge 2 ] || die "--platform requires a value"
      PLATFORM="$2"
      shift 2
      ;;
    --target)
      [ "$#" -ge 2 ] || die "--target requires a value"
      TARGET_TRIPLE="$2"
      shift 2
      ;;
    --riot-bin)
      [ "$#" -ge 2 ] || die "--riot-bin requires a value"
      RIOT_BIN="$2"
      BUILD_RIOT=0
      shift 2
      ;;
    --no-build)
      BUILD_RIOT=0
      shift
      ;;
    --no-cache)
      NO_CACHE=1
      shift
      ;;
    --progress)
      [ "$#" -ge 2 ] || die "--progress requires a value"
      PROGRESS="$2"
      shift 2
      ;;
    --arch-base)
      [ "$#" -ge 2 ] || die "--arch-base requires a value"
      ARCH_BASE_IMAGE="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      die "unknown option: $1"
      ;;
  esac
done

if [ -z "$TARGET_TRIPLE" ]; then
  case "$PLATFORM" in
    linux/arm64)
      TARGET_TRIPLE="aarch64-unknown-linux-gnu"
      ;;
    linux/amd64)
      TARGET_TRIPLE="x86_64-unknown-linux-gnu"
      ;;
    *)
      die "cannot derive target triple from platform '$PLATFORM'; pass --target"
      ;;
  esac
fi

case "$DISTRO" in
  archlinux|ubuntu)
    DOCKERFILE="$ROOT/docker/smoke/$DISTRO.Dockerfile"
    ;;
  *)
    die "unsupported distro '$DISTRO'; expected archlinux or ubuntu"
    ;;
esac

if [ "$DISTRO" = "archlinux" ] && [ -z "$ARCH_BASE_IMAGE" ]; then
  case "$PLATFORM" in
    linux/arm64)
      ARCH_BASE_IMAGE="menci/archlinuxarm:latest"
      ;;
    *)
      ARCH_BASE_IMAGE="archlinux:latest"
      ;;
  esac
fi

if [ -z "$RIOT_BIN" ]; then
  RIOT_BIN="$ROOT/_build/debug/$TARGET_TRIPLE/out/riot-cli/riot"
elif [[ "$RIOT_BIN" != /* ]]; then
  RIOT_BIN="$PWD/$RIOT_BIN"
fi

if [ "$BUILD_RIOT" -eq 1 ]; then
  (
    cd "$ROOT"
    riot build -x "$TARGET_TRIPLE" -p riot-cli
  )
fi

[ -f "$RIOT_BIN" ] || die "riot binary not found: $RIOT_BIN"
[ -x "$RIOT_BIN" ] || die "riot binary is not executable: $RIOT_BIN"
[ "$(basename "$RIOT_BIN")" = "riot" ] || die "riot binary basename must be 'riot' for the BuildKit mount"

BIN_DIR="$(cd "$(dirname "$RIOT_BIN")" && pwd)"

cmd=(
  docker buildx build
  --progress "$PROGRESS"
  --platform "$PLATFORM"
  --build-context "riot-bin=$BIN_DIR"
  -f "$DOCKERFILE"
)

if [ "$DISTRO" = "archlinux" ]; then
  cmd+=(--build-arg "ARCH_BASE_IMAGE=$ARCH_BASE_IMAGE")
fi

if [ "$NO_CACHE" -eq 1 ]; then
  cmd+=(--no-cache)
fi

cmd+=("$ROOT")

printf '==> Docker smoke: distro=%s platform=%s target=%s\n' "$DISTRO" "$PLATFORM" "$TARGET_TRIPLE"
printf '==> Riot binary: %s\n' "$RIOT_BIN"
printf '==> Dockerfile: %s\n' "$DOCKERFILE"
if [ "$DISTRO" = "archlinux" ]; then
  printf '==> Arch base image: %s\n' "$ARCH_BASE_IMAGE"
fi

"${cmd[@]}"
