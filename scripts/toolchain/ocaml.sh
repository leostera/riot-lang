#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
REPO_ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/../.." && pwd)"
VENDORED_OCAML_DIR="$REPO_ROOT/vendor/ocaml"
TARGETS_DIR="$VENDORED_OCAML_DIR/cross/targets"
DOCKERFILE="$REPO_ROOT/docker/ocaml-toolchain.Dockerfile"
ENV_FILE="${RIOT_CDN_ENV_FILE:-${OCAML_CDN_ENV_FILE:-$REPO_ROOT/.env}}"
DOCKER_CACHE_ROOT="$REPO_ROOT/.docker/volumes/ocaml"
LOCAL_CACHE_ROOT="${RIOT_OCAML_LOCAL_CACHE_ROOT:-/tmp/riot/ocaml}"
WORKTREE_LAYOUT_VERSION="2"

MODE=""
OUTPUT_ROOT="$REPO_ROOT/dist/toolchains/ocaml"
CLEAN_BUILD=0
DRY_RUN=0
TARGETS=()
RIOT_TOOLCHAIN_SUFFIX="${RIOT_TOOLCHAIN_SUFFIX:-${OCAML_TOOLCHAIN_SUFFIX:-riot.1}}"

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

usage() {
  cat <<'EOF'
Usage: ./scripts/toolchain/ocaml.sh <build|publish|release> [options] <target...>

Build or publish prebuilt OCaml toolchains from the vendored OCaml source tree.

Options:
  --output-dir PATH   Root output directory. Defaults to dist/toolchains/ocaml
  --clean             Request a clean local rebuild before packaging
  --dry-run           Print commands without executing them
  --help, -h          Show this help

Examples:
  ./scripts/toolchain/ocaml.sh build x86_64-unknown-linux-gnu
  ./scripts/toolchain/ocaml.sh publish x86_64-unknown-linux-gnu
  ./scripts/toolchain/ocaml.sh release x86_64-unknown-linux-gnu
  ./scripts/toolchain/ocaml.sh build aarch64-apple-darwin-x-x86_64-unknown-linux-gnu

Linux native GNU host targets are built via Docker Buildx:
  x86_64-unknown-linux-gnu
  aarch64-unknown-linux-gnu

Native Linux builds keep a per-target working tree cache under:
  .docker/volumes/ocaml/<target>/worktree

Other targets build in isolated local worktrees under:
  /tmp/riot/ocaml/<target>/worktree
EOF
}

die() {
  echo "error: $*" >&2
  exit 1
}

run_cmd() {
  printf '+'
  if [ "$#" -gt 0 ]; then
    printf ' %q' "$@"
  fi
  printf '\n'

  if [ "$DRY_RUN" = "0" ]; then
    "$@"
  fi
}

is_linux_host_target() {
  case "$1" in
    x86_64-unknown-linux-gnu|aarch64-unknown-linux-gnu)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

docker_platform_for_target() {
  case "$1" in
    x86_64-unknown-linux-gnu)
      printf '%s\n' "linux/amd64"
      ;;
    aarch64-unknown-linux-gnu)
      printf '%s\n' "linux/arm64"
      ;;
    *)
      die "unsupported Docker Linux host target: $1"
      ;;
  esac
}

ensure_publish_env() {
  export RIOT_TOOLCHAIN_SUFFIX
  CDN_BASE_URL="${RIOT_CDN_PUBLIC_BASE_URL:-${OCAML_CDN_PUBLIC_BASE_URL:-https://cdn.pkgs.ml}}"
  PUBLIC_BASE_URL="${CDN_BASE_URL%/}/ocaml"
  BUCKET="${RIOT_CDN_BUCKET:-${OCAML_CDN_BUCKET:-ml-pkgs-cdn}}"
  ENDPOINT_URL="${RIOT_CDN_ENDPOINT_URL:-${OCAML_CDN_ENDPOINT_URL:-}}"
  OBJECT_ACL="${RIOT_CDN_OBJECT_ACL:-${OCAML_CDN_OBJECT_ACL:-}}"
  AWS_ACCESS_KEY_ID="${AWS_ACCESS_KEY_ID:-${RIOT_CDN_ACCESS_KEY_ID:-${OCAML_CDN_ACCESS_KEY_ID:-}}}"
  AWS_SECRET_ACCESS_KEY="${AWS_SECRET_ACCESS_KEY:-${RIOT_CDN_SECRET_ACCESS_KEY:-${OCAML_CDN_SECRET_ACCESS_KEY:-}}}"
  AWS_SESSION_TOKEN="${AWS_SESSION_TOKEN:-${RIOT_CDN_SESSION_TOKEN:-${OCAML_CDN_SESSION_TOKEN:-}}}"
  AWS_REGION_VALUE="${AWS_REGION:-${AWS_DEFAULT_REGION:-${RIOT_CDN_REGION:-${OCAML_CDN_REGION:-}}}}"
  BUCKET_PREFIX="ocaml"

  if [ "$DRY_RUN" != "0" ]; then
    return 0
  fi

  command -v aws >/dev/null 2>&1 || die "aws CLI is required for publish"
  [ -n "$AWS_ACCESS_KEY_ID" ] || die "RIOT_CDN_ACCESS_KEY_ID / AWS_ACCESS_KEY_ID is required"
  [ -n "$AWS_SECRET_ACCESS_KEY" ] || die "RIOT_CDN_SECRET_ACCESS_KEY / AWS_SECRET_ACCESS_KEY is required"

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

join_object_key() {
  local prefix="$1"
  local name="$2"

  if [ -n "$prefix" ]; then
    printf '%s/%s' "${prefix%/}" "$name"
  else
    printf '%s' "$name"
  fi
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

build_linux_host_target() {
  local target="$1"
  local output_dir="$2"
  local platform
  local image_tag
  local worktree_dir

  platform="$(docker_platform_for_target "$target")"
  image_tag="riot-ocaml-toolchain:${target}"
  worktree_dir="$DOCKER_CACHE_ROOT/$target/worktree"

  command -v docker >/dev/null 2>&1 || die "docker is required for Linux host targets"
  docker buildx version >/dev/null 2>&1 || die "docker buildx is required for Linux host targets"

  if [ "$CLEAN_BUILD" != "0" ]; then
    run_cmd rm -rf "$worktree_dir"
  fi

  run_cmd mkdir -p "$worktree_dir"
  run_cmd rm -rf "$output_dir"
  run_cmd mkdir -p "$output_dir"
  run_cmd docker buildx build \
    --platform "$platform" \
    --load \
    --tag "$image_tag" \
    --file "$DOCKERFILE" \
    "$REPO_ROOT"
  run_cmd docker run \
    --rm \
    --platform "$platform" \
    --env RIOT_TOOLCHAIN_SUFFIX="$RIOT_TOOLCHAIN_SUFFIX" \
    --env OCAML_TOOLCHAIN_SUFFIX="$RIOT_TOOLCHAIN_SUFFIX" \
    --volume "$REPO_ROOT:/src:ro" \
    --volume "$worktree_dir:/work" \
    --volume "$output_dir:/out" \
    "$image_tag" \
    "$target" \
    "$CLEAN_BUILD"
}

sync_ocaml_source_tree() {
  local source_dir="$1"
  local worktree_dir="$2"

  mkdir -p "$worktree_dir"
  git -C "$source_dir" ls-files -z --cached --others --exclude-standard | \
    rsync -a --from0 --files-from=- "$source_dir"/ "$worktree_dir"/
}

reset_stale_local_worktree_if_needed() {
  local worktree_dir="$1"
  local worktree_root
  local version_file
  local current_version

  worktree_root="$(dirname "$worktree_dir")"
  version_file="$worktree_dir/.riot-worktree-layout-version"
  current_version=""

  if [ -f "$version_file" ]; then
    current_version="$(cat "$version_file" 2>/dev/null || true)"
  fi

  if [ "$current_version" = "$WORKTREE_LAYOUT_VERSION" ]; then
    return 0
  fi

  echo "Resetting cached OCaml worktree at $worktree_root"
  rm -rf "$worktree_root"
}

mark_local_worktree_layout_version() {
  local worktree_dir="$1"

  printf '%s\n' "$WORKTREE_LAYOUT_VERSION" > "$worktree_dir/.riot-worktree-layout-version"
}

host_target_for_cross_target() {
  local target="$1"

  case "$target" in
    *-x-*)
      printf '%s\n' "${target%%-x-*}"
      ;;
    *)
      return 1
      ;;
  esac
}

linux_sysroot_overlay_target() {
  local target="$1"

  case "$target" in
    *-x-aarch64-unknown-linux-gnu)
      printf '%s\n' "aarch64-unknown-linux-gnu"
      ;;
    *-x-x86_64-unknown-linux-gnu)
      printf '%s\n' "x86_64-unknown-linux-gnu"
      ;;
    *)
      return 1
      ;;
  esac
}

toolchain_sysroot_dir() {
  local toolchain_dir="$1"
  local target_triplet="$2"

  if [ -d "$toolchain_dir/gcc/$target_triplet/sysroot" ]; then
    printf '%s\n' "$toolchain_dir/gcc/$target_triplet/sysroot"
  elif [ -d "$toolchain_dir/sysroot" ]; then
    printf '%s\n' "$toolchain_dir/sysroot"
  else
    return 1
  fi
}

merge_linux_sysroot_overlay() {
  local target="$1"
  local toolchain_dir="$2"
  local overlay_target
  local sysroot_dir
  local overlay_root

  overlay_target="$(linux_sysroot_overlay_target "$target" 2>/dev/null)" || return 0
  sysroot_dir="$(toolchain_sysroot_dir "$toolchain_dir" "$overlay_target")" || \
    die "unable to locate bundled sysroot for $target in $toolchain_dir"

  echo "Merging Linux sysroot overlay for $overlay_target into $sysroot_dir"

  if [ "$DRY_RUN" != "0" ]; then
    printf '+ bash %q %q %q %q\n' \
      "$REPO_ROOT/scripts/create-sysroot.sh" \
      "$overlay_target" \
      "22.04" \
      "/tmp/riot-ocaml-sysroot.$overlay_target.XXXXXX"
    printf '+ rsync -a %q %q/\n' "/tmp/riot-ocaml-sysroot.$overlay_target.XXXXXX/sysroot-$overlay_target/" "$sysroot_dir"
    return 0
  fi

  overlay_root="$(mktemp -d "/tmp/riot-ocaml-sysroot.${overlay_target}.XXXXXX")"
  bash "$REPO_ROOT/scripts/create-sysroot.sh" "$overlay_target" "22.04" "$overlay_root"
  rsync -a "$overlay_root/sysroot-$overlay_target"/ "$sysroot_dir"/
  rm -rf "$overlay_root"
}

restore_built_host_toolchain() {
  local host_target="$1"
  local worktree_dir="$2"
  local host_output_dir="$OUTPUT_ROOT/$host_target"
  local host_tarball

  host_tarball="$(find "$host_output_dir" -maxdepth 1 -type f -name 'ocaml-*.tar.gz' | head -n 1)"
  if [ -z "$host_tarball" ]; then
    return 1
  fi

  rm -rf "$worktree_dir/cross/$host_target"
  mkdir -p "$worktree_dir/cross/$host_target"
  tar -xzf "$host_tarball" -C "$worktree_dir/cross/$host_target"
}

build_local_target() {
  local target="$1"
  local output_dir="$2"
  local worktree_dir
  local host_target
  local bootstrapped_host
  local temp_output_dir
  local tarball_path
  local final_tarball
  local checksum_path

  worktree_dir="$LOCAL_CACHE_ROOT/$target/worktree/vendor/ocaml"
  bootstrapped_host=0

  if [ "$CLEAN_BUILD" != "0" ]; then
    run_cmd rm -rf "$(dirname "$worktree_dir")"
  else
    reset_stale_local_worktree_if_needed "$worktree_dir"
  fi

  run_cmd mkdir -p "$output_dir"
  run_cmd rm -f "$output_dir"/ocaml-*.tar.gz "$output_dir"/ocaml-*.tar.gz.sha256

  if [ "$DRY_RUN" != "0" ]; then
    printf '+ sync vendored source into %q\n' "$worktree_dir"
    if host_target="$(host_target_for_cross_target "$target" 2>/dev/null)"; then
      printf '+ restore packaged host toolchain from %q when available\n' "$OUTPUT_ROOT/$host_target"
      printf '+ otherwise bootstrap host toolchain in isolated worktree: (cd %q && bash ./cross/build.sh %q)\n' "$worktree_dir" "$host_target"
    fi
    printf '+ (cd %q && bash ./cross/build.sh %q)\n' "$worktree_dir" "$target"
    temp_output_dir="$output_dir/.tmp-$target"
    printf '+ (cd %q && bash ./cross/package.sh %q %q)\n' "$worktree_dir" "$target" "$temp_output_dir"
    echo "dry-run: artifact path will be determined after packaging"
    return 0
  fi

  sync_ocaml_source_tree "$VENDORED_OCAML_DIR" "$worktree_dir"
  mark_local_worktree_layout_version "$worktree_dir"

  if host_target="$(host_target_for_cross_target "$target" 2>/dev/null)"; then
    if [ ! -d "$worktree_dir/cross/$host_target" ]; then
      if restore_built_host_toolchain "$host_target" "$worktree_dir"; then
        echo "Reused packaged host toolchain for $host_target in isolated worktree"
      else
        echo "Bootstrapping host toolchain for $target in isolated worktree"
        (
          cd "$worktree_dir"
          bash ./cross/build.sh "$host_target"
        )
        bootstrapped_host=1
      fi
    fi
  fi

  if [ "$bootstrapped_host" = "1" ]; then
    echo "Resetting source tree after host bootstrap for $target"
    (
      cd "$worktree_dir"
      make distclean
    )
  fi

  (
    cd "$worktree_dir"
    bash ./cross/build.sh "$target"
  )

  merge_linux_sysroot_overlay "$target" "$worktree_dir/cross/$target"

  temp_output_dir="$(mktemp -d "$output_dir/.tmp-${target}.XXXXXX")"
  (
    cd "$worktree_dir"
    bash ./cross/package.sh "$target" "$temp_output_dir"
  )

  tarball_path="$(find "$temp_output_dir" -maxdepth 1 -type f -name 'ocaml-*.tar.gz' | head -n 1)"
  [ -n "$tarball_path" ] || die "package step did not produce a tarball for $target"

  final_tarball="$output_dir/$(basename "$tarball_path")"
  mv -f "$tarball_path" "$final_tarball"
  checksum_path="$final_tarball.sha256"
  write_sha256_file "$final_tarball" "$checksum_path"
  rm -rf "$temp_output_dir"

  echo "  artifact: $final_tarball"
  echo "  checksum: $checksum_path"
}

publish_target() {
  local target="$1"
  local output_dir="$2"
  local tarball_path
  local checksum_path

  tarball_path="$(find "$output_dir" -maxdepth 1 -type f -name 'ocaml-*.tar.gz' | head -n 1)"
  [ -n "$tarball_path" ] || die "no tarball found to publish for $target in $output_dir"

  checksum_path="$tarball_path.sha256"
  [ -f "$checksum_path" ] || die "checksum file missing for $tarball_path"

  upload_object "$tarball_path" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$tarball_path")")"
  echo "  published: ${PUBLIC_BASE_URL%/}/$(basename "$tarball_path")"

  upload_object "$checksum_path" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$checksum_path")")"
  echo "  checksum: ${PUBLIC_BASE_URL%/}/$(basename "$checksum_path")"
}

[ $# -gt 0 ] || {
  usage >&2
  exit 1
}

case "$1" in
  build|publish|release)
    MODE="$1"
    ;;
  --help|-h)
    usage
    exit 0
    ;;
  *)
    die "unknown mode: $1"
    ;;
esac
shift

while [ $# -gt 0 ]; do
  case "$1" in
    --output-dir)
      shift
      [ $# -gt 0 ] || die "--output-dir requires a path"
      OUTPUT_ROOT="$1"
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
      TARGETS+=("$1")
      ;;
  esac
  shift
done

[ -d "$VENDORED_OCAML_DIR" ] || die "vendored OCaml source not found at $VENDORED_OCAML_DIR"
[ "${#TARGETS[@]}" -gt 0 ] || die "at least one target is required"

load_env_file "$ENV_FILE"

if [ "$MODE" = "publish" ] || [ "$MODE" = "release" ]; then
  ensure_publish_env
fi

run_cmd mkdir -p "$OUTPUT_ROOT"
run_cmd mkdir -p "$DOCKER_CACHE_ROOT"
run_cmd mkdir -p "$LOCAL_CACHE_ROOT"

for target in "${TARGETS[@]}"; do
  [ -f "$TARGETS_DIR/$target.sh" ] || die "unknown target: $target"

  target_output_dir="$OUTPUT_ROOT/$target"

  echo "======================================"
  echo " OCaml Toolchain Pipeline"
  echo "======================================"
  echo " Mode: $MODE"
  echo " Target: $target"
  echo " Output: $target_output_dir"
  echo "======================================"
  echo

  if [ "$MODE" = "build" ] || [ "$MODE" = "release" ]; then
    if is_linux_host_target "$target"; then
      build_linux_host_target "$target" "$target_output_dir"
    else
      build_local_target "$target" "$target_output_dir"
    fi
  fi

  if [ "$MODE" = "publish" ] || [ "$MODE" = "release" ]; then
    if [ "$MODE" = "release" ] && [ "$DRY_RUN" != "0" ]; then
      echo "dry-run: publish step skipped until artifacts exist"
      echo
      continue
    fi

    publish_target "$target" "$target_output_dir"
  fi

  echo
done
