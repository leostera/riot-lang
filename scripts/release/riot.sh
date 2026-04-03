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
Usage: ./scripts/release/riot.sh <target>

Build, package, and upload a riot binary plus install.sh.

Everything except the target comes from the environment.

Important environment:
  RIOT_CDN_BUCKET
  RIOT_CDN_ENDPOINT_URL
  RIOT_CDN_ACCESS_KEY_ID
  RIOT_CDN_SECRET_ACCESS_KEY

Optional environment:
  VERSION                 default: git short SHA
  BUILD_SHA               default: git short SHA (12 chars)
  OUTPUT_DIR              default: dist/riot
  INSTALL_SCRIPT_PATH     default: scripts/install.sh
  RIOT_RELEASE_NOTES_URL  default: GitHub release tag URL for v* versions
  RIOT_RELEASE_COMPARE_URL
                          default: compare current latest release to VERSION when discoverable
  RIOT_RELEASE_PREVIOUS_VERSION
                          override previous release id used for compare URL
  RIOT_RELEASE_ISSUES_URL default: https://github.com/leostera/riot/issues
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
OUTPUT_DIR="${OUTPUT_DIR:-$REPO_ROOT/dist/riot}"
CDN_BASE_URL="https://cdn.pkgs.ml"
PUBLIC_BASE_URL="${CDN_BASE_URL}/riot"
BUCKET="ml-pkgs-cdn"
ENDPOINT_URL="${RIOT_CDN_ENDPOINT_URL:-}"
OBJECT_ACL="${RIOT_CDN_OBJECT_ACL:-}"
AWS_ACCESS_KEY_ID="${RIOT_CDN_ACCESS_KEY_ID:-}"
AWS_SECRET_ACCESS_KEY="${RIOT_CDN_SECRET_ACCESS_KEY:-}"
AWS_SESSION_TOKEN="${RIOT_CDN_SESSION_TOKEN:-}"
AWS_REGION_VALUE="${RIOT_CDN_REGION:-}"
INSTALL_SCRIPT_PATH="${INSTALL_SCRIPT_PATH:-$REPO_ROOT/scripts/install.sh}"
VERSION="${VERSION:-$(git rev-parse --short HEAD)}"
BUILD_SHA="${BUILD_SHA:-$(git rev-parse --short=12 HEAD)}"
UPLOAD_ARTIFACTS="${RIOT_RELEASE_UPLOAD:-1}"
PUBLISH_LATEST="${RIOT_RELEASE_PUBLISH_LATEST:-1}"
UPLOAD_INSTALL_SCRIPT="${RIOT_RELEASE_INSTALL_SCRIPT:-1}"
DRY_RUN="${RIOT_RELEASE_DRY_RUN:-0}"
NOTES_URL="${RIOT_RELEASE_NOTES_URL:-}"
COMPARE_URL="${RIOT_RELEASE_COMPARE_URL:-}"
PREVIOUS_VERSION="${RIOT_RELEASE_PREVIOUS_VERSION:-}"
ISSUES_URL="${RIOT_RELEASE_ISSUES_URL:-https://github.com/leostera/riot/issues}"
BUCKET_PREFIX="riot"
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

download_text() {
  local url="$1"

  if command -v curl >/dev/null 2>&1; then
    curl -fsSL "$url"
    return $?
  fi

  if command -v wget >/dev/null 2>&1; then
    wget -q -O- "$url"
    return $?
  fi

  return 1
}

fetch_previous_version() {
  if [ -n "$PREVIOUS_VERSION" ]; then
    printf '%s' "$PREVIOUS_VERSION"
    return 0
  fi

  local latest_url="${PUBLIC_BASE_URL%/}/latest.json"
  local payload
  if ! payload="$(download_text "$latest_url" 2>/dev/null)"; then
    return 1
  fi

  python3 - <<'PY' "$payload"
import json
import sys

payload = sys.argv[1]
try:
    data = json.loads(payload)
except Exception:
    sys.exit(1)

release_id = data.get("release_id")
if not isinstance(release_id, str) or not release_id:
    sys.exit(1)

print(release_id, end="")
PY
}

derive_release_urls() {
  if [ -z "$NOTES_URL" ] && printf '%s' "$VERSION" | grep -Eq '^v[0-9]'; then
    NOTES_URL="https://github.com/leostera/riot/releases/tag/$VERSION"
  fi

  if [ -z "$COMPARE_URL" ]; then
    local previous_version
    if previous_version="$(fetch_previous_version)"; then
      if [ -n "$previous_version" ] && [ "$previous_version" != "$VERSION" ]; then
        COMPARE_URL="https://github.com/leostera/riot/compare/$previous_version...$VERSION"
      fi
    fi
  fi
}

write_release_metadata() {
  local output_path="$1"

  python3 - <<'PY' "$output_path" "$VERSION" "$BUILD_SHA" "$NOTES_URL" "$COMPARE_URL" "$ISSUES_URL"
import json
import pathlib
import sys

output_path, release_id, build_sha, notes_url, compare_url, issues_url = sys.argv[1:]

payload = {
    "release_id": release_id,
    "build_sha": build_sha,
    "notes_url": notes_url or None,
    "compare_url": compare_url or None,
    "issues_url": issues_url or None,
}

path = pathlib.Path(output_path)
path.parent.mkdir(parents=True, exist_ok=True)
path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
PY
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
derive_release_urls

HOST_TARGET="$(detect_host_triple)"
RELEASE_RIOT="$REPO_ROOT/riot"
[ -x "$RELEASE_RIOT" ] || die "expected release driver at $RELEASE_RIOT"

if [ "$TARGET" = "$HOST_TARGET" ]; then
  run_cmd "$RELEASE_RIOT" build --release riot-cli
else
  run_cmd "$RELEASE_RIOT" build --release -x "$TARGET" riot-cli
fi

BINARY_PATH="$REPO_ROOT/_build/release/$TARGET/out/riot-cli/riot"
VERSIONED_TARBALL="$OUTPUT_DIR/riot-${VERSION}-${TARGET}.tar.gz"
LATEST_TARBALL="$OUTPUT_DIR/riot-latest-${TARGET}.tar.gz"
VERSIONED_METADATA="$OUTPUT_DIR/riot-${VERSION}.json"
LATEST_METADATA="$OUTPUT_DIR/latest.json"
STAGING_DIR="$OUTPUT_DIR/.pkg-$TARGET"

write_release_metadata "$VERSIONED_METADATA"
if [ "$PUBLISH_LATEST" != "0" ]; then
  if [ "$DRY_RUN" = "1" ]; then
    run_cmd cp "$VERSIONED_METADATA" "$LATEST_METADATA"
  else
    cp "$VERSIONED_METADATA" "$LATEST_METADATA"
  fi
fi

if [ "$DRY_RUN" = "1" ]; then
  run_cmd mkdir -p "$STAGING_DIR"
  run_cmd cp "$BINARY_PATH" "$STAGING_DIR/riot"
  run_cmd cp "$VERSIONED_METADATA" "$STAGING_DIR/release.json"
  run_cmd chmod +x "$STAGING_DIR/riot"
  run_cmd tar czf "$VERSIONED_TARBALL" -C "$STAGING_DIR" riot release.json
  if [ "$PUBLISH_LATEST" != "0" ]; then
    run_cmd cp "$VERSIONED_TARBALL" "$LATEST_TARBALL"
  fi
else
  [ -f "$BINARY_PATH" ] || die "expected built binary at $BINARY_PATH"
  rm -rf "$STAGING_DIR"
  mkdir -p "$STAGING_DIR"
  cp "$BINARY_PATH" "$STAGING_DIR/riot"
  cp "$VERSIONED_METADATA" "$STAGING_DIR/release.json"
  chmod +x "$STAGING_DIR/riot"
  tar czf "$VERSIONED_TARBALL" -C "$STAGING_DIR" riot release.json
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
upload_object "$VERSIONED_METADATA" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$VERSIONED_METADATA")")" --content-type "application/json"
echo "  published: ${PUBLIC_BASE_URL%/}/$(basename "$VERSIONED_TARBALL")"
echo "  metadata: ${PUBLIC_BASE_URL%/}/$(basename "$VERSIONED_METADATA")"

if [ "$PUBLISH_LATEST" != "0" ]; then
  upload_object "$LATEST_TARBALL" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$LATEST_TARBALL")")"
  upload_object "$LATEST_TARBALL.sha256" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$LATEST_TARBALL").sha256")"
  upload_object "$LATEST_METADATA" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$LATEST_METADATA")")" --content-type "application/json"
  echo "  alias: ${PUBLIC_BASE_URL%/}/$(basename "$LATEST_TARBALL")"
  echo "  latest: ${PUBLIC_BASE_URL%/}/$(basename "$LATEST_METADATA")"
fi

if [ "$UPLOAD_INSTALL_SCRIPT" != "0" ]; then
  upload_object "$INSTALL_SCRIPT_PATH" "$INSTALL_SCRIPT_KEY" --content-type "text/x-shellscript"
  echo "  install: ${PUBLIC_BASE_URL%/}/install.sh"
fi
