#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
REPO_ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/../.." && pwd)"
VENDORED_OCAML_DIR="$REPO_ROOT/vendor/ocaml"
TARGETS_DIR="$VENDORED_OCAML_DIR/cross/targets"
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

usage() {
  cat <<'EOF'
Usage: ./scripts/release/ocaml.sh <target>

Build, package, and upload a prebuilt OCaml toolchain.

Everything except the target comes from the environment.

Important environment:
  RIOT_CDN_BUCKET
  RIOT_CDN_ENDPOINT_URL
  RIOT_CDN_ACCESS_KEY_ID
  RIOT_CDN_SECRET_ACCESS_KEY

Optional environment:
  OUTPUT_DIR              default: artifacts/ocaml
  RIOT_CDN_PUBLIC_BASE_URL
                          default: https://cdn.ocaml.ai
  RIOT_CDN_OBJECT_ACL
  RIOT_RELEASE_UPLOAD     default: 1
  RIOT_RELEASE_CLEAN      default: 0
  RIOT_RELEASE_DRY_RUN    default: 0
EOF
}

if [ $# -ne 1 ] || [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
  usage
  [ $# -eq 1 ] && exit 0
  exit 1
fi

TARGET="$1"
OUTPUT_DIR="${OUTPUT_DIR:-$REPO_ROOT/artifacts/ocaml}"
CDN_BASE_URL="${RIOT_CDN_PUBLIC_BASE_URL:-${OCAML_CDN_PUBLIC_BASE_URL:-https://cdn.ocaml.ai}}"
PUBLIC_BASE_URL="${CDN_BASE_URL%/}/ocaml"
BUCKET="${RIOT_CDN_BUCKET:-${OCAML_CDN_BUCKET:-}}"
ENDPOINT_URL="${RIOT_CDN_ENDPOINT_URL:-${OCAML_CDN_ENDPOINT_URL:-}}"
OBJECT_ACL="${RIOT_CDN_OBJECT_ACL:-${OCAML_CDN_OBJECT_ACL:-}}"
AWS_ACCESS_KEY_ID="${AWS_ACCESS_KEY_ID:-${RIOT_CDN_ACCESS_KEY_ID:-${OCAML_CDN_ACCESS_KEY_ID:-}}}"
AWS_SECRET_ACCESS_KEY="${AWS_SECRET_ACCESS_KEY:-${RIOT_CDN_SECRET_ACCESS_KEY:-${OCAML_CDN_SECRET_ACCESS_KEY:-}}}"
AWS_SESSION_TOKEN="${AWS_SESSION_TOKEN:-${RIOT_CDN_SESSION_TOKEN:-${OCAML_CDN_SESSION_TOKEN:-}}}"
AWS_REGION_VALUE="${AWS_REGION:-${AWS_DEFAULT_REGION:-${RIOT_CDN_REGION:-${OCAML_CDN_REGION:-}}}}"
UPLOAD_ARTIFACTS="${RIOT_RELEASE_UPLOAD:-1}"
CLEAN_BUILD="${RIOT_RELEASE_CLEAN:-0}"
DRY_RUN="${RIOT_RELEASE_DRY_RUN:-0}"
BUCKET_PREFIX="ocaml"

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

upload_object() {
  local source_path="$1"
  local object_key="$2"

  local upload_args
  upload_args=(s3 cp "$source_path" "s3://$BUCKET/$object_key")
  if [ -n "$ENDPOINT_URL" ]; then
    upload_args+=(--endpoint-url "$ENDPOINT_URL")
  fi
  if [ -n "$OBJECT_ACL" ]; then
    upload_args+=(--acl "$OBJECT_ACL")
  fi

  run_cmd aws "${upload_args[@]}"
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

[ -d "$VENDORED_OCAML_DIR" ] || die "vendored OCaml source not found at $VENDORED_OCAML_DIR"
[ -f "$TARGETS_DIR/$TARGET.sh" ] || die "unknown target: $TARGET"

if [ "$UPLOAD_ARTIFACTS" != "0" ]; then
  command -v aws >/dev/null 2>&1 || die "aws CLI is required for uploads"
  [ -n "$BUCKET" ] || die "RIOT_CDN_BUCKET / OCAML_CDN_BUCKET is required"
  [ -n "$AWS_ACCESS_KEY_ID" ] || die "RIOT_CDN_ACCESS_KEY_ID / AWS_ACCESS_KEY_ID is required"
  [ -n "$AWS_SECRET_ACCESS_KEY" ] || die "RIOT_CDN_SECRET_ACCESS_KEY / AWS_SECRET_ACCESS_KEY is required"
  configure_aws_env
fi

run_cmd mkdir -p "$OUTPUT_DIR"

BUILD_ARGS=("$VENDORED_OCAML_DIR/cross/build.sh" "$TARGET")
if [ "$CLEAN_BUILD" != "0" ]; then
  BUILD_ARGS+=("--clean")
fi
run_cmd "${BUILD_ARGS[@]}"

if [ "$DRY_RUN" = "1" ]; then
  TEMP_OUTPUT_DIR="$OUTPUT_DIR/.tmp-$TARGET"
  run_cmd mkdir -p "$TEMP_OUTPUT_DIR"
  run_cmd "$VENDORED_OCAML_DIR/cross/package.sh" "$TARGET" "$TEMP_OUTPUT_DIR"
  echo "dry-run: artifact path will be determined after packaging"
  exit 0
fi

TEMP_OUTPUT_DIR="$(mktemp -d "$OUTPUT_DIR/.tmp-${TARGET}.XXXXXX")"
"$VENDORED_OCAML_DIR/cross/package.sh" "$TARGET" "$TEMP_OUTPUT_DIR"

TARBALL_PATH="$(find "$TEMP_OUTPUT_DIR" -maxdepth 1 -type f -name 'ocaml-*.tar.gz' | head -n 1)"
[ -n "$TARBALL_PATH" ] || die "package step did not produce a tarball for $TARGET"

FINAL_TARBALL="$OUTPUT_DIR/$(basename "$TARBALL_PATH")"
mv -f "$TARBALL_PATH" "$FINAL_TARBALL"
CHECKSUM_PATH="$FINAL_TARBALL.sha256"
write_sha256_file "$FINAL_TARBALL" "$CHECKSUM_PATH"
rm -rf "$TEMP_OUTPUT_DIR"

echo "  artifact: $FINAL_TARBALL"
echo "  checksum: $CHECKSUM_PATH"

if [ "$UPLOAD_ARTIFACTS" = "0" ]; then
  echo "Artifacts kept locally in: $OUTPUT_DIR"
  exit 0
fi

upload_object "$FINAL_TARBALL" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$FINAL_TARBALL")")"
echo "  published: ${PUBLIC_BASE_URL%/}/$(basename "$FINAL_TARBALL")"

upload_object "$CHECKSUM_PATH" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$CHECKSUM_PATH")")"
echo "  checksum: ${PUBLIC_BASE_URL%/}/$(basename "$CHECKSUM_PATH")"
