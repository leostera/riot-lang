#!/bin/bash
# Manual publish script for the riot-builder image.
#
# This is the interim path while the GitHub Actions publishing workflow remains
# disabled. It builds the image locally, smoke-tests it, tags it for GHCR, and
# optionally pushes it.

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

LOCAL_IMAGE_NAME="${LOCAL_IMAGE_NAME:-riot-builder}"
LOCAL_IMAGE_TAG="${LOCAL_IMAGE_TAG:-local}"
REMOTE_IMAGE="${REMOTE_IMAGE:-ghcr.io/leostera/riot/riot-builder}"
REMOTE_TAG="${REMOTE_TAG:-latest}"
DOCKERFILE="${DOCKERFILE:-docker/Dockerfile}"
PLATFORM="${PLATFORM:-}"
PUSH_IMAGE=1
BUILD_IMAGE=1
RUN_SMOKE_TEST=1
DRY_RUN=0

usage() {
  cat <<EOF
Usage: $0 [OPTIONS]

Build, test, tag, and optionally push the riot-builder image.

Options:
  --remote-image IMAGE   Remote image name (default: ${REMOTE_IMAGE})
  --remote-tag TAG       Remote tag to publish (default: ${REMOTE_TAG})
  --local-name NAME      Local image name (default: ${LOCAL_IMAGE_NAME})
  --local-tag TAG        Local image tag (default: ${LOCAL_IMAGE_TAG})
  --dockerfile PATH      Dockerfile to build (default: ${DOCKERFILE})
  --platform PLATFORM    Build platform (for example: linux/amd64)
  --no-build             Skip local image build and reuse existing local image
  --no-smoke-test        Skip smoke-test commands
  --no-push              Tag the image but do not push it
  --dry-run              Print commands without running them
  --help                 Show this help text

This script also tags the image with the current git short SHA.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --remote-image)
      REMOTE_IMAGE="$2"
      shift 2
      ;;
    --remote-tag)
      REMOTE_TAG="$2"
      shift 2
      ;;
    --local-name)
      LOCAL_IMAGE_NAME="$2"
      shift 2
      ;;
    --local-tag)
      LOCAL_IMAGE_TAG="$2"
      shift 2
      ;;
    --dockerfile)
      DOCKERFILE="$2"
      shift 2
      ;;
    --platform)
      PLATFORM="$2"
      shift 2
      ;;
    --no-build)
      BUILD_IMAGE=0
      shift
      ;;
    --no-smoke-test)
      RUN_SMOKE_TEST=0
      shift
      ;;
    --no-push)
      PUSH_IMAGE=0
      shift
      ;;
    --dry-run)
      DRY_RUN=1
      shift
      ;;
    --help)
      usage
      exit 0
      ;;
    *)
      echo -e "${RED}Unknown option: $1${NC}"
      usage
      exit 1
      ;;
  esac
done

if [[ ! -f "bootstrap.py" ]]; then
  echo -e "${RED}Error: bootstrap.py not found.${NC}"
  echo "Run this script from the Riot repository root."
  exit 1
fi

SHORT_SHA="$(git rev-parse --short HEAD)"
LOCAL_REF="${LOCAL_IMAGE_NAME}:${LOCAL_IMAGE_TAG}"
REMOTE_REF="${REMOTE_IMAGE}:${REMOTE_TAG}"
REMOTE_SHA_REF="${REMOTE_IMAGE}:sha-${SHORT_SHA}"

run_cmd() {
  echo "+ $*"
  if [[ "${DRY_RUN}" -eq 0 ]]; then
    "$@"
  fi
}

echo -e "${GREEN}=== Publishing Riot Docker Image ===${NC}"
echo "Local image:  ${LOCAL_REF}"
echo "Remote image: ${REMOTE_REF}"
echo "SHA tag:      ${REMOTE_SHA_REF}"
if [[ -n "${PLATFORM}" ]]; then
  echo "Platform:     ${PLATFORM}"
fi
echo ""

if [[ "${BUILD_IMAGE}" -eq 1 ]]; then
  BUILD_ARGS=(./docker/build.sh --name "${LOCAL_IMAGE_NAME}" --tag "${LOCAL_IMAGE_TAG}")
  if [[ -n "${PLATFORM}" ]]; then
    BUILD_ARGS+=(--platform "${PLATFORM}")
  fi
  run_cmd "${BUILD_ARGS[@]}"
fi

if [[ "${RUN_SMOKE_TEST}" -eq 1 ]]; then
  run_cmd docker run --rm "${LOCAL_REF}" --help
  run_cmd docker run --rm -v "$(pwd):/app" "${LOCAL_REF}" build riot-cli
fi

run_cmd docker tag "${LOCAL_REF}" "${REMOTE_REF}"
run_cmd docker tag "${LOCAL_REF}" "${REMOTE_SHA_REF}"

if [[ "${PUSH_IMAGE}" -eq 1 ]]; then
  echo -e "${YELLOW}Pushing tags...${NC}"
  run_cmd docker push "${REMOTE_REF}"
  run_cmd docker push "${REMOTE_SHA_REF}"
else
  echo -e "${YELLOW}Skipping push (--no-push).${NC}"
fi

echo ""
echo -e "${GREEN}Done.${NC}"
echo "Published refs:"
echo "  ${REMOTE_REF}"
echo "  ${REMOTE_SHA_REF}"
