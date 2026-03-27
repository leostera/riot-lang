#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
REPO_ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/../.." && pwd)"
ENV_FILE="${RIOT_CDN_ENV_FILE:-${OCAML_CDN_ENV_FILE:-$REPO_ROOT/.env}}"

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

OUTPUT_DIR="${OUTPUT_DIR:-$REPO_ROOT/artifacts/tusk}"
PUBLIC_BASE_URL="${RIOT_CDN_PUBLIC_BASE_URL:-${OCAML_CDN_PUBLIC_BASE_URL:-https://cdn.ocaml.ai/tusk}}"
BUCKET="${RIOT_CDN_BUCKET:-${OCAML_CDN_BUCKET:-}}"
ENDPOINT_URL="${RIOT_CDN_ENDPOINT_URL:-${OCAML_CDN_ENDPOINT_URL:-}}"
BUCKET_PREFIX="${RIOT_CDN_BUCKET_PREFIX:-${OCAML_CDN_BUCKET_PREFIX:-tusk}}"
OBJECT_ACL="${RIOT_CDN_OBJECT_ACL:-${OCAML_CDN_OBJECT_ACL:-}}"
AWS_ACCESS_KEY_ID="${AWS_ACCESS_KEY_ID:-${RIOT_CDN_ACCESS_KEY_ID:-${OCAML_CDN_ACCESS_KEY_ID:-}}}"
AWS_SECRET_ACCESS_KEY="${AWS_SECRET_ACCESS_KEY:-${RIOT_CDN_SECRET_ACCESS_KEY:-${OCAML_CDN_SECRET_ACCESS_KEY:-}}}"
AWS_SESSION_TOKEN="${AWS_SESSION_TOKEN:-${RIOT_CDN_SESSION_TOKEN:-${OCAML_CDN_SESSION_TOKEN:-}}}"
AWS_REGION_VALUE="${AWS_REGION:-${AWS_DEFAULT_REGION:-${RIOT_CDN_REGION:-${OCAML_CDN_REGION:-}}}}"
INSTALL_SCRIPT_PATH="${INSTALL_SCRIPT_PATH:-$REPO_ROOT/scripts/install.sh}"
INSTALL_SCRIPT_KEY="${INSTALL_SCRIPT_KEY:-}"

VERSION="${VERSION:-$(git rev-parse --short HEAD)}"
RUN_BOOTSTRAP=1
BUILD_TUSK=1
UPLOAD_ARTIFACTS=1
UPLOAD_INSTALL_SCRIPT=1
PUBLISH_LATEST=1
DRY_RUN=0

targets=()

usage() {
  cat <<EOF
Usage: $0 [OPTIONS] [target ...]

Build, package, and optionally upload tusk binaries plus the install.sh script.

If no targets are provided, the host target is used.

Options:
  --version VALUE        Version string for tarballs
                         (default: current git short SHA)
  --output-dir PATH      Output directory for generated artifacts
                         (default: $OUTPUT_DIR)
  --bucket NAME          S3/R2 bucket name
                         (default: \$RIOT_CDN_BUCKET or \$OCAML_CDN_BUCKET)
  --endpoint-url URL     S3-compatible endpoint URL
                         (default: \$RIOT_CDN_ENDPOINT_URL or \$OCAML_CDN_ENDPOINT_URL)
  --bucket-prefix PATH   Object key prefix for tusk artifacts
                         (default: $BUCKET_PREFIX)
  --public-base-url URL  Public base URL for printed artifact links
                         (default: $PUBLIC_BASE_URL)
  --install-script PATH  Install script to upload
                         (default: $INSTALL_SCRIPT_PATH)
  --install-script-key   Object key for the uploaded install script
                         (default: <bucket-prefix>/install.sh)
  --acl ACL              Optional canned ACL to pass to aws s3 cp
  --no-bootstrap         Skip ./bootstrap.py and ./minitusk
  --no-build             Skip tusk-cli builds and only package/upload existing outputs
  --no-upload            Skip upload and keep artifacts locally
  --no-install-script    Do not upload install.sh
  --no-latest           Skip publishing tusk-latest-<target>.tar.gz aliases
  --dry-run              Print commands without executing them
  --help                 Show this help text

Environment:
  RIOT_CDN_BUCKET / OCAML_CDN_BUCKET
  RIOT_CDN_ENDPOINT_URL / OCAML_CDN_ENDPOINT_URL
  RIOT_CDN_BUCKET_PREFIX / OCAML_CDN_BUCKET_PREFIX
  RIOT_CDN_PUBLIC_BASE_URL / OCAML_CDN_PUBLIC_BASE_URL
  RIOT_CDN_OBJECT_ACL / OCAML_CDN_OBJECT_ACL
  RIOT_CDN_ACCESS_KEY_ID / OCAML_CDN_ACCESS_KEY_ID
  RIOT_CDN_SECRET_ACCESS_KEY / OCAML_CDN_SECRET_ACCESS_KEY
  RIOT_CDN_SESSION_TOKEN / OCAML_CDN_SESSION_TOKEN
  RIOT_CDN_REGION / OCAML_CDN_REGION
  RIOT_CDN_ENV_FILE / OCAML_CDN_ENV_FILE
EOF
}

die() {
  echo "error: $*" >&2
  exit 1
}

run_cmd() {
  echo "+ $*"
  if [ "$DRY_RUN" -eq 0 ]; then
    "$@"
  fi
}

join_object_key() {
  local prefix="$1"
  local name="$2"

  prefix="${prefix#/}"
  prefix="${prefix%/}"

  if [ -n "$prefix" ]; then
    printf '%s/%s' "$prefix" "$name"
  else
    printf '%s' "$name"
  fi
}

configure_aws_env() {
  if [ -n "$AWS_ACCESS_KEY_ID" ]; then
    export AWS_ACCESS_KEY_ID
  fi
  if [ -n "$AWS_SECRET_ACCESS_KEY" ]; then
    export AWS_SECRET_ACCESS_KEY
  fi
  if [ -n "$AWS_SESSION_TOKEN" ]; then
    export AWS_SESSION_TOKEN
  fi

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
  elif command -v python3 >/dev/null 2>&1; then
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
  else
    die "need one of: sha256sum, shasum, or python3 to generate checksums"
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
    *)
      die "unsupported operating system: $os"
      ;;
  esac
}

build_target() {
  local target="$1"
  local host_target="$2"
  local bootstrap_tusk="$REPO_ROOT/_build/bootstrap/out/Tusk_cli/tusk"

  if [ "$target" = "$host_target" ]; then
    run_cmd "$bootstrap_tusk" build --no-code-server tusk-cli
  else
    run_cmd "$bootstrap_tusk" build --no-code-server -x "$target" tusk-cli
  fi
}

package_target() {
  local target="$1"
  local binary_path="$REPO_ROOT/_build/debug/$target/out/tusk-cli/tusk"
  local versioned_tarball="$OUTPUT_DIR/tusk-${VERSION}-${target}.tar.gz"
  local latest_tarball="$OUTPUT_DIR/tusk-latest-${target}.tar.gz"
  local staging_dir="$OUTPUT_DIR/.pkg-$target"

  if [ "$DRY_RUN" -eq 1 ]; then
    run_cmd mkdir -p "$staging_dir"
    run_cmd cp "$binary_path" "$staging_dir/tusk"
    run_cmd chmod +x "$staging_dir/tusk"
    run_cmd tar czf "$versioned_tarball" -C "$staging_dir" tusk
    if [ "$PUBLISH_LATEST" -eq 1 ]; then
      run_cmd cp "$versioned_tarball" "$latest_tarball"
    fi
    return 0
  fi

  [ -f "$binary_path" ] || die "expected built binary at $binary_path"

  rm -rf "$staging_dir"
  mkdir -p "$staging_dir"
  cp "$binary_path" "$staging_dir/tusk"
  chmod +x "$staging_dir/tusk"

  tar czf "$versioned_tarball" -C "$staging_dir" tusk
  write_sha256_file "$versioned_tarball" "$versioned_tarball.sha256"

  if [ "$PUBLISH_LATEST" -eq 1 ]; then
    cp "$versioned_tarball" "$latest_tarball"
    cp "$versioned_tarball.sha256" "$latest_tarball.sha256"
  fi

  rm -rf "$staging_dir"
}

upload_object() {
  local source_path="$1"
  local object_key="$2"
  shift 2

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

while [ $# -gt 0 ]; do
  case "$1" in
    --version)
      shift
      [ $# -gt 0 ] || die "--version requires a value"
      VERSION="$1"
      ;;
    --output-dir)
      shift
      [ $# -gt 0 ] || die "--output-dir requires a value"
      OUTPUT_DIR="$1"
      ;;
    --bucket)
      shift
      [ $# -gt 0 ] || die "--bucket requires a value"
      BUCKET="$1"
      ;;
    --endpoint-url)
      shift
      [ $# -gt 0 ] || die "--endpoint-url requires a value"
      ENDPOINT_URL="$1"
      ;;
    --bucket-prefix)
      shift
      [ $# -gt 0 ] || die "--bucket-prefix requires a value"
      BUCKET_PREFIX="$1"
      ;;
    --public-base-url)
      shift
      [ $# -gt 0 ] || die "--public-base-url requires a value"
      PUBLIC_BASE_URL="$1"
      ;;
    --install-script)
      shift
      [ $# -gt 0 ] || die "--install-script requires a value"
      INSTALL_SCRIPT_PATH="$1"
      ;;
    --install-script-key)
      shift
      [ $# -gt 0 ] || die "--install-script-key requires a value"
      INSTALL_SCRIPT_KEY="$1"
      ;;
    --acl)
      shift
      [ $# -gt 0 ] || die "--acl requires a value"
      OBJECT_ACL="$1"
      ;;
    --no-bootstrap)
      RUN_BOOTSTRAP=0
      ;;
    --no-build)
      BUILD_TUSK=0
      ;;
    --no-upload)
      UPLOAD_ARTIFACTS=0
      ;;
    --no-install-script)
      UPLOAD_INSTALL_SCRIPT=0
      ;;
    --no-latest)
      PUBLISH_LATEST=0
      ;;
    --dry-run)
      DRY_RUN=1
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    --*)
      die "unknown option: $1"
      ;;
    *)
      targets+=("$1")
      ;;
  esac
  shift
done

cd "$REPO_ROOT"

host_target="$(detect_host_triple)"
if [ "${#targets[@]}" -eq 0 ]; then
  targets=("$host_target")
fi

if [ -z "$INSTALL_SCRIPT_KEY" ]; then
  INSTALL_SCRIPT_KEY="$(join_object_key "$BUCKET_PREFIX" "install.sh")"
fi

[ -f "$INSTALL_SCRIPT_PATH" ] || die "install script not found at $INSTALL_SCRIPT_PATH"

if [ "$UPLOAD_ARTIFACTS" -eq 1 ]; then
  command -v aws >/dev/null 2>&1 || die "aws CLI is required for uploads"
  [ -n "$BUCKET" ] || die "--bucket or RIOT_CDN_BUCKET / OCAML_CDN_BUCKET is required for uploads"
  configure_aws_env
  [ -n "${AWS_ACCESS_KEY_ID:-}" ] || die "RIOT_CDN_ACCESS_KEY_ID / AWS_ACCESS_KEY_ID is required for uploads"
  [ -n "${AWS_SECRET_ACCESS_KEY:-}" ] || die "RIOT_CDN_SECRET_ACCESS_KEY / AWS_SECRET_ACCESS_KEY is required for uploads"
fi

run_cmd mkdir -p "$OUTPUT_DIR"

if [ "$RUN_BOOTSTRAP" -eq 1 ]; then
  run_cmd ./bootstrap.py
  run_cmd ./minitusk
fi

for target in "${targets[@]}"; do
  echo "==> Releasing target: $target"
  if [ "$BUILD_TUSK" -eq 1 ]; then
    build_target "$target" "$host_target"
  fi
  package_target "$target"
done

if [ "$DRY_RUN" -eq 1 ]; then
  if [ "$UPLOAD_INSTALL_SCRIPT" -eq 1 ] && [ "$UPLOAD_ARTIFACTS" -eq 1 ]; then
    upload_object "$INSTALL_SCRIPT_PATH" "$INSTALL_SCRIPT_KEY" --content-type "text/x-shellscript"
  fi
  exit 0
fi

if [ "$UPLOAD_ARTIFACTS" -eq 0 ]; then
  echo ""
  echo "Artifacts kept locally in: $OUTPUT_DIR"
  exit 0
fi

for target in "${targets[@]}"; do
  versioned_tarball="$OUTPUT_DIR/tusk-${VERSION}-${target}.tar.gz"
  upload_object "$versioned_tarball" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$versioned_tarball")")"
  upload_object "$versioned_tarball.sha256" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$versioned_tarball").sha256")"
  echo "  published: ${PUBLIC_BASE_URL%/}/$(basename "$versioned_tarball")"

  if [ "$PUBLISH_LATEST" -eq 1 ]; then
    latest_tarball="$OUTPUT_DIR/tusk-latest-${target}.tar.gz"
    upload_object "$latest_tarball" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$latest_tarball")")"
    upload_object "$latest_tarball.sha256" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$latest_tarball").sha256")"
    echo "  alias: ${PUBLIC_BASE_URL%/}/$(basename "$latest_tarball")"
  fi
done

if [ "$UPLOAD_INSTALL_SCRIPT" -eq 1 ]; then
  upload_object "$INSTALL_SCRIPT_PATH" "$INSTALL_SCRIPT_KEY" --content-type "text/x-shellscript"
  echo "  install: ${PUBLIC_BASE_URL%/}/install.sh"
fi
