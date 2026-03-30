#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
REPO_ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/../.." && pwd)"
ENV_FILE="$REPO_ROOT/.env"

load_env_file() {
  local env_file="$1"

  if [ ! -f "$env_file" ]; then
    return 0
  fi

  echo "==> Loading environment from $env_file"

  set +u
  set -a
  # shellcheck disable=SC1090
  . "$env_file"
  set +a
  set -u
}

load_env_file "$ENV_FILE"

usage() {
  cat <<'EOF'
Usage: ./scripts/release/tusk.sh <target>

Build, package, and upload a tusk binary plus install.sh.

Everything except the target comes from the environment.

Important environment:
  RIOT_CDN_BUCKET
  RIOT_CDN_ENDPOINT_URL
  RIOT_CDN_ACCESS_KEY_ID
  RIOT_CDN_SECRET_ACCESS_KEY

Optional environment:
  VERSION                 default: git short SHA
  OUTPUT_DIR              default: dist/tusk
  INSTALL_SCRIPT_PATH     default: scripts/install.sh
  RIOT_RELEASE_UPLOAD     default: 1
  RIOT_RELEASE_PUBLISH_LATEST
                          default: 1
  RIOT_RELEASE_INSTALL_SCRIPT
                          default: 1
  RIOT_RELEASE_DRY_RUN    default: 0
  RIOT_CDN_ENDPOINT_URL
  RIOT_CDN_ACCESS_KEY_ID
  RIOT_CDN_SECRET_ACCESS_KEY
  RIOT_CDN_SESSION_TOKEN
  RIOT_CDN_REGION
  RIOT_CDN_OBJECT_ACL
EOF
}

if [ $# -ne 1 ] || [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
  usage
  [ $# -eq 1 ] && exit 0
  exit 1
fi

TARGET="$1"
OUTPUT_DIR="${OUTPUT_DIR:-$REPO_ROOT/dist/tusk}"
CDN_BASE_URL="https://cdn.pkgs.ml"
PUBLIC_BASE_URL="${CDN_BASE_URL}/tusk"
BUCKET="ml-pkgs-cdn"
ENDPOINT_URL="${RIOT_CDN_ENDPOINT_URL:-}"
OBJECT_ACL="${RIOT_CDN_OBJECT_ACL:-}"
AWS_ACCESS_KEY_ID="${RIOT_CDN_ACCESS_KEY_ID:-}"
AWS_SECRET_ACCESS_KEY="${RIOT_CDN_SECRET_ACCESS_KEY:-}"
AWS_SESSION_TOKEN="${RIOT_CDN_SESSION_TOKEN:-}"
AWS_REGION_VALUE="${RIOT_CDN_REGION:-}"
INSTALL_SCRIPT_PATH="${INSTALL_SCRIPT_PATH:-$REPO_ROOT/scripts/install.sh}"
VERSION="${VERSION:-$(git rev-parse --short HEAD)}"
UPLOAD_ARTIFACTS="${RIOT_RELEASE_UPLOAD:-1}"
PUBLISH_LATEST="${RIOT_RELEASE_PUBLISH_LATEST:-1}"
UPLOAD_INSTALL_SCRIPT="${RIOT_RELEASE_INSTALL_SCRIPT:-1}"
DRY_RUN="${RIOT_RELEASE_DRY_RUN:-0}"
BUCKET_PREFIX="tusk"
INSTALL_SCRIPT_KEY="$BUCKET_PREFIX/install.sh"

die() {
  echo "error: $*" >&2
  exit 1
}

run_cmd() {
  echo "+ $*"
  if [ "$DRY_RUN" = "0" ]; then
    "$@"
  fi
}

join_object_key() {
  local prefix="$1"
  local name="$2"

  if [ -n "$prefix" ]; then
    printf '%s/%s' "${prefix%/}" "$name"
  else
    printf '%s' "$name"
  fi
}

configure_aws_env() {
  [ -n "$AWS_ACCESS_KEY_ID" ] && export AWS_ACCESS_KEY_ID
  [ -n "$AWS_SECRET_ACCESS_KEY" ] && export AWS_SECRET_ACCESS_KEY
  [ -n "$AWS_SESSION_TOKEN" ] && export AWS_SESSION_TOKEN

  if [ -z "$AWS_REGION_VALUE" ] && printf '%s' "$ENDPOINT_URL" | grep -q 'cloudflarestorage.com'; then
    AWS_REGION_VALUE="auto"
  fi

  if [ -n "$AWS_REGION_VALUE" ]; then
    export AWS_REGION="$AWS_REGION_VALUE"
    export AWS_DEFAULT_REGION="$AWS_REGION_VALUE"
  fi

  export AWS_EC2_METADATA_DISABLED="${AWS_EC2_METADATA_DISABLED:-true}"
}

write_sha256_file() {
  local artifact_path="$1"
  local checksum_path="$2"
  local artifact_dir
  local artifact_name

  artifact_dir="$(dirname "$artifact_path")"
  artifact_name="$(basename "$artifact_path")"

  if command -v sha256sum >/dev/null 2>&1; then
    (
      cd "$artifact_dir"
      sha256sum "$artifact_name" > "$checksum_path"
    )
  elif command -v shasum >/dev/null 2>&1; then
    (
      cd "$artifact_dir"
      shasum -a 256 "$artifact_name" > "$checksum_path"
    )
  else
    python3 - "$artifact_path" > "$checksum_path" <<'PY'
import hashlib
import os
import sys

path = sys.argv[1]
h = hashlib.sha256()
with open(path, "rb") as f:
    for chunk in iter(lambda: f.read(1024 * 1024), b""):
        h.update(chunk)
print(f"{h.hexdigest()}  {os.path.basename(path)}")
PY
  fi
}

detect_host_triple() {
  local os arch libc

  os="$(uname -s)"
  arch="$(uname -m)"

  case "$arch" in
    x86_64|amd64) arch="x86_64" ;;
    aarch64|arm64) arch="aarch64" ;;
    *) die "unsupported architecture: $arch" ;;
  esac

  case "$os" in
    Darwin) echo "${arch}-apple-darwin" ;;
    Linux)
      if ldd --version 2>&1 | grep -qi musl; then
        libc="musl"
      else
        libc="gnu"
      fi
      echo "${arch}-unknown-linux-${libc}"
      ;;
    *) die "unsupported operating system: $os" ;;
  esac
}

upload_object() {
  local source_path="$1"
  local object_key="$2"
  shift 2

  local upload_args
  upload_args=(s3 cp "$source_path" "s3://$BUCKET/$object_key")
  if [ -n "$ENDPOINT_URL" ]; then
    upload_args+=(--endpoint-url "$ENDPOINT_URL")
  fi
  if [ -n "$OBJECT_ACL" ]; then
    upload_args+=(--acl "$OBJECT_ACL")
  fi
  if [ "$#" -gt 0 ]; then
    upload_args+=("$@")
  fi

  run_cmd aws "${upload_args[@]}"
}

[ -f "$INSTALL_SCRIPT_PATH" ] || die "install script not found at $INSTALL_SCRIPT_PATH"

if [ "$UPLOAD_ARTIFACTS" != "0" ]; then
  command -v aws >/dev/null 2>&1 || die "aws CLI is required for uploads"
  [ -n "$AWS_ACCESS_KEY_ID" ] || die "RIOT_CDN_ACCESS_KEY_ID is required"
  [ -n "$AWS_SECRET_ACCESS_KEY" ] || die "RIOT_CDN_SECRET_ACCESS_KEY is required"
  configure_aws_env
fi

cd "$REPO_ROOT"
run_cmd mkdir -p "$OUTPUT_DIR"

HOST_TARGET="$(detect_host_triple)"
RELEASE_TUSK="$REPO_ROOT/tusk"
[ -x "$RELEASE_TUSK" ] || die "expected release driver at $RELEASE_TUSK"

if [ "$TARGET" = "$HOST_TARGET" ]; then
  run_cmd "$RELEASE_TUSK" build tusk-cli
else
  run_cmd "$RELEASE_TUSK" build -x "$TARGET" tusk-cli
fi

BINARY_PATH="$REPO_ROOT/_build/debug/$TARGET/out/tusk-cli/tusk"
VERSIONED_TARBALL="$OUTPUT_DIR/tusk-${VERSION}-${TARGET}.tar.gz"
LATEST_TARBALL="$OUTPUT_DIR/tusk-latest-${TARGET}.tar.gz"
STAGING_DIR="$OUTPUT_DIR/.pkg-$TARGET"

if [ "$DRY_RUN" = "1" ]; then
  run_cmd mkdir -p "$STAGING_DIR"
  run_cmd cp "$BINARY_PATH" "$STAGING_DIR/tusk"
  run_cmd chmod +x "$STAGING_DIR/tusk"
  run_cmd tar czf "$VERSIONED_TARBALL" -C "$STAGING_DIR" tusk
  if [ "$PUBLISH_LATEST" != "0" ]; then
    run_cmd cp "$VERSIONED_TARBALL" "$LATEST_TARBALL"
  fi
else
  [ -f "$BINARY_PATH" ] || die "expected built binary at $BINARY_PATH"
  rm -rf "$STAGING_DIR"
  mkdir -p "$STAGING_DIR"
  cp "$BINARY_PATH" "$STAGING_DIR/tusk"
  chmod +x "$STAGING_DIR/tusk"
  tar czf "$VERSIONED_TARBALL" -C "$STAGING_DIR" tusk
  write_sha256_file "$VERSIONED_TARBALL" "$VERSIONED_TARBALL.sha256"
  if [ "$PUBLISH_LATEST" != "0" ]; then
    cp "$VERSIONED_TARBALL" "$LATEST_TARBALL"
    cp "$VERSIONED_TARBALL.sha256" "$LATEST_TARBALL.sha256"
  fi
  rm -rf "$STAGING_DIR"
fi

if [ "$UPLOAD_ARTIFACTS" = "0" ]; then
  echo "Artifacts kept locally in: $OUTPUT_DIR"
  exit 0
fi

upload_object "$VERSIONED_TARBALL" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$VERSIONED_TARBALL")")"
upload_object "$VERSIONED_TARBALL.sha256" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$VERSIONED_TARBALL").sha256")"
echo "  published: ${PUBLIC_BASE_URL%/}/$(basename "$VERSIONED_TARBALL")"

if [ "$PUBLISH_LATEST" != "0" ]; then
  upload_object "$LATEST_TARBALL" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$LATEST_TARBALL")")"
  upload_object "$LATEST_TARBALL.sha256" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$LATEST_TARBALL").sha256")"
  echo "  alias: ${PUBLIC_BASE_URL%/}/$(basename "$LATEST_TARBALL")"
fi

if [ "$UPLOAD_INSTALL_SCRIPT" != "0" ]; then
  upload_object "$INSTALL_SCRIPT_PATH" "$INSTALL_SCRIPT_KEY" --content-type "text/x-shellscript"
  echo "  install: ${PUBLIC_BASE_URL%/}/install.sh"
fi
