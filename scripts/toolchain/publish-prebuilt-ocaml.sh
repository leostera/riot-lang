#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
REPO_ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/../.." && pwd)"
VENDORED_OCAML_DIR="$REPO_ROOT/vendor/ocaml"
TARGETS_DIR="$VENDORED_OCAML_DIR/cross/targets"

OUTPUT_DIR="${OUTPUT_DIR:-$REPO_ROOT/artifacts/ocaml}"
PUBLIC_BASE_URL="${OCAML_CDN_PUBLIC_BASE_URL:-https://cdn.ocaml.ai/ocaml}"
BUCKET="${OCAML_CDN_BUCKET:-}"
ENDPOINT_URL="${OCAML_CDN_ENDPOINT_URL:-}"
BUCKET_PREFIX="${OCAML_CDN_BUCKET_PREFIX:-ocaml}"
OBJECT_ACL="${OCAML_CDN_OBJECT_ACL:-}"

BUILD_COMPILERS=1
UPLOAD_ARTIFACTS=1
CLEAN_BUILD=0
DRY_RUN=0

targets=()

usage() {
  cat <<EOF
Usage: $0 [OPTIONS] <target> [<target> ...]

Build, package, and optionally upload selected vendored OCaml compilers.

Targets are vendored target names from vendor/ocaml/cross/targets, for example:
  aarch64-apple-darwin
  aarch64-unknown-linux-gnu
  aarch64-apple-darwin-x-x86_64-unknown-linux-gnu

Options:
  --output-dir PATH       Output directory for tarballs
                          (default: $OUTPUT_DIR)
  --bucket NAME           S3/R2 bucket name
                          (default: \$OCAML_CDN_BUCKET)
  --endpoint-url URL      S3-compatible endpoint URL
                          (default: \$OCAML_CDN_ENDPOINT_URL)
  --bucket-prefix PREFIX  Object key prefix inside the bucket
                          (default: $BUCKET_PREFIX)
  --public-base-url URL   Public base URL for printed artifact links
                          (default: $PUBLIC_BASE_URL)
  --acl ACL               Optional canned ACL to pass to aws s3 cp
                          (default: unset)
  --no-build              Skip compiler builds and only package/upload
  --no-upload             Skip upload and keep artifacts locally
  --clean                 Run vendored builds with --clean
  --dry-run               Print commands without executing them
  --help                  Show this help text

Environment:
  OCAML_CDN_BUCKET
  OCAML_CDN_ENDPOINT_URL
  OCAML_CDN_BUCKET_PREFIX
  OCAML_CDN_PUBLIC_BASE_URL
  OCAML_CDN_OBJECT_ACL

Examples:
  $0 --no-upload aarch64-apple-darwin
  $0 --bucket riot-artifacts --endpoint-url https://<account>.r2.cloudflarestorage.com \\
     aarch64-apple-darwin aarch64-unknown-linux-gnu
EOF
}

list_available_targets() {
  find "$TARGETS_DIR" -maxdepth 1 -type f -name '*.sh' -print \
    | sort \
    | sed 's#^.*/##' \
    | sed 's/\.sh$//'
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

while [ $# -gt 0 ]; do
  case "$1" in
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
    --acl)
      shift
      [ $# -gt 0 ] || die "--acl requires a value"
      OBJECT_ACL="$1"
      ;;
    --no-build)
      BUILD_COMPILERS=0
      ;;
    --no-upload)
      UPLOAD_ARTIFACTS=0
      ;;
    --clean)
      CLEAN_BUILD=1
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

[ -d "$VENDORED_OCAML_DIR" ] || die "vendored OCaml source not found at $VENDORED_OCAML_DIR"

if [ "${#targets[@]}" -eq 0 ]; then
  echo "Available targets:" >&2
  list_available_targets >&2
  die "at least one target is required"
fi

for target in "${targets[@]}"; do
  [ -f "$TARGETS_DIR/$target.sh" ] || die "unknown target: $target"
done

if [ "$UPLOAD_ARTIFACTS" -eq 1 ]; then
  command -v aws >/dev/null 2>&1 || die "aws CLI is required for uploads"
  [ -n "$BUCKET" ] || die "--bucket or OCAML_CDN_BUCKET is required for uploads"
fi

run_cmd mkdir -p "$OUTPUT_DIR"

artifacts=()
checksums=()

for target in "${targets[@]}"; do
  echo "==> Processing target: $target"

  if [ "$BUILD_COMPILERS" -eq 1 ]; then
    build_args=("$VENDORED_OCAML_DIR/cross/build.sh" "$target")
    if [ "$CLEAN_BUILD" -eq 1 ]; then
      build_args+=("--clean")
    fi
    run_cmd "${build_args[@]}"
  fi

  if [ "$DRY_RUN" -eq 1 ]; then
    temp_output_dir="$OUTPUT_DIR/.tmp-$target"
    run_cmd mkdir -p "$temp_output_dir"
    run_cmd "$VENDORED_OCAML_DIR/cross/package.sh" "$target" "$temp_output_dir"
    echo "  dry-run: artifact path will be determined after packaging"
    continue
  fi

  temp_output_dir="$(mktemp -d "$OUTPUT_DIR/.tmp-${target}.XXXXXX")"
  "$VENDORED_OCAML_DIR/cross/package.sh" "$target" "$temp_output_dir"

  tarball_path="$(find "$temp_output_dir" -maxdepth 1 -type f -name 'ocaml-*.tar.gz' | head -n 1)"
  [ -n "$tarball_path" ] || die "package step did not produce a tarball for $target"

  final_tarball="$OUTPUT_DIR/$(basename "$tarball_path")"
  mv -f "$tarball_path" "$final_tarball"

  checksum_path="$final_tarball.sha256"
  write_sha256_file "$final_tarball" "$checksum_path"

  rm -rf "$temp_output_dir"

  artifacts+=("$final_tarball")
  checksums+=("$checksum_path")

  echo "  artifact: $final_tarball"
  echo "  checksum: $checksum_path"
done

if [ "$DRY_RUN" -eq 1 ]; then
  exit 0
fi

if [ "$UPLOAD_ARTIFACTS" -eq 0 ]; then
  echo ""
  echo "Artifacts kept locally in: $OUTPUT_DIR"
  exit 0
fi

for artifact_path in "${artifacts[@]}"; do
  object_key="${BUCKET_PREFIX%/}/$(basename "$artifact_path")"
  upload_args=(s3 cp "$artifact_path" "s3://$BUCKET/$object_key")
  if [ -n "$ENDPOINT_URL" ]; then
    upload_args+=(--endpoint-url "$ENDPOINT_URL")
  fi
  if [ -n "$OBJECT_ACL" ]; then
    upload_args+=(--acl "$OBJECT_ACL")
  fi

  run_cmd aws "${upload_args[@]}"
  echo "  published: ${PUBLIC_BASE_URL%/}/$(basename "$artifact_path")"
done

for checksum_path in "${checksums[@]}"; do
  object_key="${BUCKET_PREFIX%/}/$(basename "$checksum_path")"
  upload_args=(s3 cp "$checksum_path" "s3://$BUCKET/$object_key")
  if [ -n "$ENDPOINT_URL" ]; then
    upload_args+=(--endpoint-url "$ENDPOINT_URL")
  fi
  if [ -n "$OBJECT_ACL" ]; then
    upload_args+=(--acl "$OBJECT_ACL")
  fi

  run_cmd aws "${upload_args[@]}"
  echo "  checksum: ${PUBLIC_BASE_URL%/}/$(basename "$checksum_path")"
done
