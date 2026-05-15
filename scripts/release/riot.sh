#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
REPO_ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/../.." && pwd)"
ENV_FILE="$REPO_ROOT/.env"
RIOT_CLI_MANIFEST_PATH="$REPO_ROOT/packages/riot-cli/riot.toml"
OCAML_TOOLCHAIN_CONFIG_PATH="$REPO_ROOT/ocaml-toolchain.toml"

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
Usage: ./scripts/release/riot.sh [--force] <target|all>

Build, package, and upload a riot binary plus install.sh.

The release version comes from ./packages/riot-cli/riot.toml.
If riot/manifest.json already contains that version, the script aborts before
building so an old release is not republished accidentally.
Pass --force to bypass that remote-version guard.

When <target> is "all", the script releases every enabled target from
./ocaml-toolchain.toml. If that file is missing, it falls back to the host
target plus any matching vendor/ocaml cross targets for that host.

Examples:
  ./scripts/release/riot.sh --force aarch64-apple-darwin
  ./scripts/release/riot.sh aarch64-apple-darwin
  ./scripts/release/riot.sh all

Important environment:
  RIOT_CDN_BUCKET
  RIOT_CDN_ENDPOINT_URL
  RIOT_CDN_ACCESS_KEY_ID
  RIOT_CDN_SECRET_ACCESS_KEY

Optional environment:
  BUILD_SHA               default: git short SHA (12 chars)
  OUTPUT_DIR              default: dist/riot
  INSTALL_SCRIPT_PATH     default: scripts/install.sh
  RIOT_RELEASE_RIOT_BIN   default: command -v riot
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
  RIOT_RELEASE_TARGET_DIR default: _build
  RIOT_RELEASE_DRY_RUN    default: 0
  RIOT_CDN_ENDPOINT_URL
  RIOT_CDN_ACCESS_KEY_ID
  RIOT_CDN_SECRET_ACCESS_KEY
  RIOT_CDN_SESSION_TOKEN
  RIOT_CDN_REGION
  RIOT_CDN_OBJECT_ACL
EOF
}

read_riot_cli_version() {
  local manifest_path="${1:-$RIOT_CLI_MANIFEST_PATH}"

  if [ ! -f "$manifest_path" ]; then
    echo "error: riot-cli manifest not found at $manifest_path" >&2
    exit 1
  fi

  python3 - <<'PY' "$manifest_path"
import sys

manifest_path = sys.argv[1]

try:
    import tomllib
except Exception as exc:
    raise SystemExit(f"error: python tomllib is required to read {manifest_path}: {exc}")

with open(manifest_path, "rb") as manifest_file:
    data = tomllib.load(manifest_file)

package = data.get("package")
if not isinstance(package, dict):
    raise SystemExit(f"error: missing [package] table in {manifest_path}")

version = package.get("version")
if not isinstance(version, str) or not version:
    raise SystemExit(f"error: missing package.version in {manifest_path}")

print(version, end="")
PY
}

read_toolchain_version() {
  local config_path="${1:-$OCAML_TOOLCHAIN_CONFIG_PATH}"

  if [ ! -f "$config_path" ]; then
    return 0
  fi

  python3 - <<'PY' "$config_path"
import sys

config_path = sys.argv[1]

try:
    import tomllib
except Exception:
    sys.exit(0)

try:
    with open(config_path, "rb") as config_file:
        data = tomllib.load(config_file)
except Exception:
    sys.exit(0)

toolchain = data.get("toolchain")
if not isinstance(toolchain, dict):
    sys.exit(0)

version = toolchain.get("version")
if isinstance(version, str) and version:
    print(version, end="")
PY
}

FORCE_RELEASE=0
POSITIONAL_ARGS=()

while [ $# -gt 0 ]; do
  case "$1" in
    --help|-h)
      usage
      exit 0
      ;;
    --force)
      FORCE_RELEASE=1
      ;;
    --*)
      echo "error: unknown argument: $1" >&2
      usage >&2
      exit 1
      ;;
    *)
      POSITIONAL_ARGS+=("$1")
      ;;
  esac
  shift
done

if [ "${#POSITIONAL_ARGS[@]}" -ne 1 ]; then
  usage >&2
  exit 1
fi

REQUESTED_TARGET="${POSITIONAL_ARGS[0]}"
OUTPUT_DIR="${OUTPUT_DIR:-$REPO_ROOT/dist/riot}"
CDN_BASE_URL="https://cdn.pkgs.ml"
PUBLIC_BASE_URL="${CDN_BASE_URL}/riot"
METADATA_BASE_URL="https://cdn.pkgs.ml"
BUCKET="ml-pkgs-cdn"
ENDPOINT_URL="${RIOT_CDN_ENDPOINT_URL:-}"
OBJECT_ACL="${RIOT_CDN_OBJECT_ACL:-}"
AWS_ACCESS_KEY_ID="${RIOT_CDN_ACCESS_KEY_ID:-}"
AWS_SECRET_ACCESS_KEY="${RIOT_CDN_SECRET_ACCESS_KEY:-}"
AWS_SESSION_TOKEN="${RIOT_CDN_SESSION_TOKEN:-}"
AWS_REGION_VALUE="${RIOT_CDN_REGION:-}"
INSTALL_SCRIPT_PATH="${INSTALL_SCRIPT_PATH:-$REPO_ROOT/scripts/install.sh}"
VERSION="$(read_riot_cli_version)"
TOOLCHAIN_VERSION="${TOOLCHAIN_VERSION:-$(read_toolchain_version)}"
BUILD_SHA="${BUILD_SHA:-$(git rev-parse --short=12 HEAD)}"
UPLOAD_ARTIFACTS="${RIOT_RELEASE_UPLOAD:-1}"
PUBLISH_LATEST="${RIOT_RELEASE_PUBLISH_LATEST:-1}"
UPLOAD_INSTALL_SCRIPT="${RIOT_RELEASE_INSTALL_SCRIPT:-1}"
DRY_RUN="${RIOT_RELEASE_DRY_RUN:-0}"
BUILD_TARGET_DIR="${RIOT_RELEASE_TARGET_DIR:-_build}"
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

resolve_target_dir_root() {
  local target_dir="$1"

  if [[ "$target_dir" = /* ]]; then
    printf '%s\n' "$target_dir"
  else
    printf '%s\n' "$REPO_ROOT/$target_dir"
  fi
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

  local latest_url="${METADATA_BASE_URL%/}/riot/latest.json"
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

list_bucket_objects_json() {
  local output_path="$1"
  local prefix
  local aws_args

  prefix="${BUCKET_PREFIX%/}/"
  aws_args=(s3api list-objects-v2 --bucket "$BUCKET" --prefix "$prefix" --output json)
  if [ -n "$ENDPOINT_URL" ]; then
    aws_args+=(--endpoint-url "$ENDPOINT_URL")
  fi

  printf '+'
  printf ' %q' aws "${aws_args[@]}"
  printf ' > %q\n' "$output_path"

  aws "${aws_args[@]}" > "$output_path"
}

generate_remote_manifest_json() {
  local output_path="$1"
  local listing_path="${2:-}"
  local cleanup_listing=0

  if [ -z "$listing_path" ]; then
    listing_path="$(mktemp "/tmp/riot-release-objects.XXXXXX")"
    cleanup_listing=1
    list_bucket_objects_json "$listing_path"
  fi

  printf '+'
  printf ' %q' python3 - "$listing_path" "$output_path" "$PUBLIC_BASE_URL" "$BUCKET_PREFIX"
  printf '\n'

  python3 - "$listing_path" "$output_path" "$PUBLIC_BASE_URL" "$BUCKET_PREFIX" <<'PY'
import json
import re
import sys
from datetime import datetime, timezone

listing_path, output_path, public_base_url, bucket_prefix = sys.argv[1:]

with open(listing_path, "r", encoding="utf-8") as listing_file:
    listing = json.load(listing_file)

contents = listing.get("Contents") or []
target_re = re.compile(
    r"(?P<target>"
    r"(?:aarch64|x86_64)-apple-darwin|"
    r"(?:aarch64|x86_64)-unknown-linux-(?:gnu|musl)|"
    r"(?:aarch64|x86_64)-w64-mingw32"
    r")$"
)

def parse_release(entry):
    key = entry.get("Key", "")
    if not key.startswith(f"{bucket_prefix}/"):
      return None

    artifact = key.split("/", 1)[1]
    if not artifact.startswith("riot-") or not artifact.endswith(".tar.gz"):
      return None
    if artifact.startswith("riot-latest-"):
      return None

    base = artifact[:-len(".tar.gz")]
    match = target_re.search(base)
    if match is None:
      return None

    target = match.group("target")
    prefix = f"riot-"
    version_end = len(base) - len(target) - 1
    version = base[len(prefix):version_end]
    if not version:
      return None

    return {
      "version": version,
      "target": target,
      "artifact": artifact,
      "artifact_url": f"{public_base_url.rstrip('/')}/{artifact}",
      "checksum_url": f"{public_base_url.rstrip('/')}/{artifact}.sha256",
      "metadata_url": f"{public_base_url.rstrip('/')}/riot-{version}.json",
      "size_bytes": entry.get("Size"),
      "last_modified": entry.get("LastModified"),
    }

releases = {}
seen_artifacts = set()
for entry in contents:
    parsed = parse_release(entry)
    if parsed is None:
        continue
    artifact_name = parsed["artifact"]
    if artifact_name in seen_artifacts:
        continue
    seen_artifacts.add(artifact_name)
    version = parsed["version"]
    releases.setdefault(version, {
        "version": version,
        "metadata_url": parsed["metadata_url"],
        "targets": [],
    })["targets"].append({
        "target": parsed["target"],
        "artifact": parsed["artifact"],
        "artifact_url": parsed["artifact_url"],
        "checksum_url": parsed["checksum_url"],
        "size_bytes": parsed["size_bytes"],
        "last_modified": parsed["last_modified"],
    })

for release in releases.values():
    release["targets"].sort(key=lambda item: item["target"])

manifest = {
    "schema_version": 1,
    "generated_at": datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
    "base_url": public_base_url.rstrip("/"),
    "versions": sorted(releases),
    "releases": [releases[version] for version in sorted(releases)],
}

with open(output_path, "w", encoding="utf-8") as output_file:
    json.dump(manifest, output_file, indent=2)
    output_file.write("\n")
PY

  if [ "$cleanup_listing" = "1" ]; then
    rm -f "$listing_path"
  fi
}

remote_manifest_has_version() {
  local manifest_path="$1"
  local version="$2"

  python3 - "$manifest_path" "$version" <<'PY'
import json
import sys

manifest_path, version = sys.argv[1:]

with open(manifest_path, "r", encoding="utf-8") as manifest_file:
    manifest = json.load(manifest_file)

versions = manifest.get("versions")
if not isinstance(versions, list):
    print("0", end="")
    raise SystemExit(0)

print("1" if version in versions else "0", end="")
PY
}

ensure_release_version_is_new() {
  if [ "$FORCE_RELEASE" = "1" ]; then
    echo "==> Skipping remote version guard for $VERSION (--force)"
    return 0
  fi

  local listing_path
  local manifest_path
  local exists

  listing_path="$(mktemp "/tmp/riot-release-objects.XXXXXX")"
  manifest_path="$(mktemp "/tmp/riot-release-manifest.XXXXXX")"

  list_bucket_objects_json "$listing_path"
  generate_remote_manifest_json "$manifest_path" "$listing_path"
  exists="$(remote_manifest_has_version "$manifest_path" "$VERSION")"

  rm -f "$listing_path" "$manifest_path"

  if [ "$exists" = "1" ]; then
    die "release version $VERSION already exists in ${PUBLIC_BASE_URL%/}/manifest.json"
  fi
}

publish_remote_manifest() {
  local manifest_path

  manifest_path="$(mktemp "/tmp/riot-release-manifest.XXXXXX")"
  generate_remote_manifest_json "$manifest_path"
  upload_object "$manifest_path" "$(join_object_key "$BUCKET_PREFIX" "manifest.json")" --content-type "application/json"
  echo "  manifest: ${PUBLIC_BASE_URL%/}/manifest.json"
  rm -f "$manifest_path"
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

toolchain_root_for_target() {
  local target="$1"

  if [ -z "$TOOLCHAIN_VERSION" ]; then
    return 1
  fi

  printf '%s\n' "$HOME/.riot/toolchains/$TOOLCHAIN_VERSION/$target"
}

target_strip_prefixes() {
  local target="$1"

  case "$target" in
    aarch64-unknown-linux-gnu)
      printf '%s\n' "aarch64-linux-gnu-" "aarch64-unknown-linux-gnu-"
      ;;
    x86_64-unknown-linux-gnu)
      printf '%s\n' "x86_64-linux-gnu-" "x86_64-unknown-linux-gnu-"
      ;;
    aarch64-unknown-linux-musl)
      printf '%s\n' "aarch64-linux-musl-" "aarch64-unknown-linux-musl-"
      ;;
    x86_64-unknown-linux-musl)
      printf '%s\n' "x86_64-linux-musl-" "x86_64-unknown-linux-musl-"
      ;;
    x86_64-w64-mingw32)
      printf '%s\n' "x86_64-w64-mingw32-"
      ;;
    aarch64-w64-mingw32)
      printf '%s\n' "aarch64-w64-mingw32-"
      ;;
    *)
      return 1
      ;;
  esac
}

find_strip_tool() {
  local target="$1"
  local host_target="$2"
  local toolchain_root=""
  local prefix=""
  local candidate=""

  if [ "$target" = "$host_target" ]; then
    if command -v strip >/dev/null 2>&1; then
      command -v strip
      return 0
    fi
    return 1
  fi

  toolchain_root="$(toolchain_root_for_target "$target" 2>/dev/null || true)"
  while IFS= read -r prefix; do
    [ -n "$prefix" ] || continue

    if [ -n "$toolchain_root" ]; then
      for candidate in \
        "$toolchain_root/bin/${prefix}strip" \
        "$toolchain_root/gcc/bin/${prefix}strip"
      do
        if [ -x "$candidate" ]; then
          printf '%s\n' "$candidate"
          return 0
        fi
      done
    fi

    if command -v "${prefix}strip" >/dev/null 2>&1; then
      command -v "${prefix}strip"
      return 0
    fi
  done < <(target_strip_prefixes "$target" 2>/dev/null || true)

  return 1
}

strip_binary() {
  local target="$1"
  local host_target="$2"
  local binary_path="$3"
  local strip_tool=""

  strip_tool="$(find_strip_tool "$target" "$host_target")" || \
    die "unable to find strip tool for target $target"

  if [[ "$target" == *-apple-darwin ]]; then
    run_cmd "$strip_tool" -S -x "$binary_path"
  else
    run_cmd "$strip_tool" "$binary_path"
  fi
}

toolchain_config_targets() {
  local config_path="$REPO_ROOT/ocaml-toolchain.toml"

  [ -f "$config_path" ] || return 0

  python3 - <<'PY' "$config_path"
import sys

config_path = sys.argv[1]

try:
    import tomllib
except Exception:
    sys.exit(0)

try:
    with open(config_path, "rb") as f:
        data = tomllib.load(f)
except Exception:
    sys.exit(0)

toolchain = data.get("toolchain")
if not isinstance(toolchain, dict):
    sys.exit(0)

targets = toolchain.get("targets")
if not isinstance(targets, list):
    sys.exit(0)

for target in targets:
    if isinstance(target, str) and target:
        print(target)
PY
}

release_targets_for_host() {
  local host_target="$1"
  local configured_targets=()
  local fallback_targets=()
  local target=""
  local target_file=""
  local seen=0

  configured_targets+=("$host_target")
  while IFS= read -r target; do
    [ -n "$target" ] || continue
    configured_targets+=("$target")
  done < <(toolchain_config_targets)

  if [ "${#configured_targets[@]}" -gt 1 ]; then
    for target in "${configured_targets[@]}"; do
      seen=0
      for existing in "${fallback_targets[@]:-}"; do
        if [ "$existing" = "$target" ]; then
          seen=1
          break
        fi
      done
      if [ "$seen" = "0" ]; then
        fallback_targets+=("$target")
      fi
    done
    printf '%s\n' "${fallback_targets[@]}"
    return 0
  fi

  fallback_targets=("$host_target")
  while IFS= read -r target_file; do
    [ -n "$target_file" ] || continue
    target="$(basename "$target_file" .sh)"
    target="${target#${host_target}-x-}"
    fallback_targets+=("$target")
  done < <(find "$REPO_ROOT/vendor/ocaml/cross/targets" -maxdepth 1 -type f -name "${host_target}-x-*.sh" | sort)

  printf '%s\n' "${fallback_targets[@]}"
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
  ensure_release_version_is_new
fi

cd "$REPO_ROOT"
run_cmd mkdir -p "$OUTPUT_DIR"
derive_release_urls

HOST_TARGET="$(detect_host_triple)"
TARGETS=()
if [ "$REQUESTED_TARGET" = "all" ]; then
  while IFS= read -r target; do
    [ -n "$target" ] || continue
    TARGETS+=("$target")
  done < <(release_targets_for_host "$HOST_TARGET")
else
  TARGETS=("$REQUESTED_TARGET")
fi

if [ "${#TARGETS[@]}" -eq 0 ]; then
  die "no releasable targets found for host $HOST_TARGET"
fi

RELEASE_RIOT="${RIOT_RELEASE_RIOT_BIN:-$(command -v riot || true)}"
[ -n "$RELEASE_RIOT" ] || die "expected an installed riot binary in PATH or RIOT_RELEASE_RIOT_BIN"
[ -x "$RELEASE_RIOT" ] || die "release driver is not executable: $RELEASE_RIOT"
BUILD_TARGET_DIR_ROOT="$(resolve_target_dir_root "$BUILD_TARGET_DIR")"

VERSIONED_METADATA="$OUTPUT_DIR/riot-${VERSION}.json"
LATEST_METADATA="$OUTPUT_DIR/latest.json"

write_release_metadata "$VERSIONED_METADATA"
if [ "$PUBLISH_LATEST" != "0" ]; then
  if [ "$DRY_RUN" = "1" ]; then
    run_cmd cp "$VERSIONED_METADATA" "$LATEST_METADATA"
  else
    cp "$VERSIONED_METADATA" "$LATEST_METADATA"
  fi
fi

for TARGET in "${TARGETS[@]}"; do
  BUILD_ARGS=(build --release --target-dir "$BUILD_TARGET_DIR")
  if [ "$TARGET" = "$HOST_TARGET" ]; then
    run_cmd "$RELEASE_RIOT" "${BUILD_ARGS[@]}" -p riot-cli
  else
    run_cmd "$RELEASE_RIOT" "${BUILD_ARGS[@]}" -x "$TARGET" -p riot-cli
  fi

  BINARY_PATH="$BUILD_TARGET_DIR_ROOT/release/$TARGET/out/riot-cli/riot"
  VERSIONED_TARBALL="$OUTPUT_DIR/riot-${VERSION}-${TARGET}.tar.gz"
  LATEST_TARBALL="$OUTPUT_DIR/riot-latest-${TARGET}.tar.gz"
  STAGING_DIR="$OUTPUT_DIR/.pkg-$TARGET"

  if [ "$DRY_RUN" = "1" ]; then
    run_cmd mkdir -p "$STAGING_DIR"
    run_cmd cp "$BINARY_PATH" "$STAGING_DIR/riot"
    run_cmd cp "$VERSIONED_METADATA" "$STAGING_DIR/release.json"
    strip_binary "$TARGET" "$HOST_TARGET" "$STAGING_DIR/riot"
    run_cmd chmod +x "$STAGING_DIR/riot"
    run_cmd env COPYFILE_DISABLE=1 tar czf "$VERSIONED_TARBALL" -C "$STAGING_DIR" riot release.json
    if [ "$PUBLISH_LATEST" != "0" ]; then
      run_cmd cp "$VERSIONED_TARBALL" "$LATEST_TARBALL"
    fi
  else
    [ -f "$BINARY_PATH" ] || die "expected built binary at $BINARY_PATH"
    rm -rf "$STAGING_DIR"
    mkdir -p "$STAGING_DIR"
    cp "$BINARY_PATH" "$STAGING_DIR/riot"
    cp "$VERSIONED_METADATA" "$STAGING_DIR/release.json"
    strip_binary "$TARGET" "$HOST_TARGET" "$STAGING_DIR/riot"
    chmod +x "$STAGING_DIR/riot"
    env COPYFILE_DISABLE=1 tar czf "$VERSIONED_TARBALL" -C "$STAGING_DIR" riot release.json
    write_sha256_file "$VERSIONED_TARBALL" "$VERSIONED_TARBALL.sha256"
    if [ "$PUBLISH_LATEST" != "0" ]; then
      cp "$VERSIONED_TARBALL" "$LATEST_TARBALL"
      cp "$VERSIONED_TARBALL.sha256" "$LATEST_TARBALL.sha256"
    fi
    rm -rf "$STAGING_DIR"
  fi

  if [ "$UPLOAD_ARTIFACTS" != "0" ]; then
    upload_object "$VERSIONED_TARBALL" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$VERSIONED_TARBALL")")"
    upload_object "$VERSIONED_TARBALL.sha256" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$VERSIONED_TARBALL").sha256")"
    echo "  published: ${PUBLIC_BASE_URL%/}/$(basename "$VERSIONED_TARBALL")"

    if [ "$PUBLISH_LATEST" != "0" ]; then
      upload_object "$LATEST_TARBALL" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$LATEST_TARBALL")")"
      upload_object "$LATEST_TARBALL.sha256" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$LATEST_TARBALL").sha256")"
      echo "  alias: ${PUBLIC_BASE_URL%/}/$(basename "$LATEST_TARBALL")"
    fi
  fi
done

if [ "$UPLOAD_ARTIFACTS" = "0" ]; then
  echo "Artifacts kept locally in: $OUTPUT_DIR"
  exit 0
fi

upload_object "$VERSIONED_METADATA" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$VERSIONED_METADATA")")" --content-type "application/json"
echo "  metadata: ${PUBLIC_BASE_URL%/}/$(basename "$VERSIONED_METADATA")"

if [ "$PUBLISH_LATEST" != "0" ]; then
  upload_object "$LATEST_METADATA" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$LATEST_METADATA")")" --content-type "application/json"
  echo "  latest: ${METADATA_BASE_URL%/}/riot/$(basename "$LATEST_METADATA")"
fi

if [ "$UPLOAD_INSTALL_SCRIPT" != "0" ]; then
  upload_object "$INSTALL_SCRIPT_PATH" "$INSTALL_SCRIPT_KEY" --content-type "text/x-shellscript"
  echo "  install: ${PUBLIC_BASE_URL%/}/install.sh"
fi

publish_remote_manifest
