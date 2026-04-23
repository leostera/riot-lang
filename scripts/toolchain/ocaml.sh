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
WORKTREE_LAYOUT_VERSION="3"

MODE=""
OUTPUT_ROOT="$REPO_ROOT/dist/toolchains/ocaml"
CLEAN_BUILD=0
DRY_RUN=0
TARGETS=()
CLI_TOOLCHAIN_SUFFIX=""
INITIAL_RIOT_TOOLCHAIN_SUFFIX="${RIOT_TOOLCHAIN_SUFFIX:-}"
INITIAL_OCAML_TOOLCHAIN_SUFFIX="${OCAML_TOOLCHAIN_SUFFIX:-}"

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
Usage:
  ./scripts/toolchain/ocaml.sh build [options] <target...>
  ./scripts/toolchain/ocaml.sh publish [options] <suffix> <target...>
  ./scripts/toolchain/ocaml.sh release [options] <suffix> <target...>

Build or publish prebuilt OCaml toolchains from the vendored OCaml source tree.

Options:
  --output-dir PATH   Root output directory. Defaults to dist/toolchains/ocaml
  --suffix VALUE      Release suffix to append to the OCaml version (for example riot.3)
  --clean             Request a clean local rebuild before packaging
  --dry-run           Print commands without executing them
  --help, -h          Show this help

Examples:
  ./scripts/toolchain/ocaml.sh build x86_64-unknown-linux-gnu
  ./scripts/toolchain/ocaml.sh build all
  ./scripts/toolchain/ocaml.sh publish riot.3 x86_64-unknown-linux-gnu
  ./scripts/toolchain/ocaml.sh publish riot.3 all
  ./scripts/toolchain/ocaml.sh publish --clean riot.3 x86_64-unknown-linux-gnu
  ./scripts/toolchain/ocaml.sh release riot.3 x86_64-unknown-linux-gnu
  ./scripts/toolchain/ocaml.sh build aarch64-apple-darwin-x-x86_64-unknown-linux-gnu

Linux native GNU host targets are built via Docker Buildx:
  x86_64-unknown-linux-gnu
  aarch64-unknown-linux-gnu

Targets whose build host is one of those Linux GNU toolchains also build via
Docker when invoked from non-Linux hosts. For example:
  x86_64-unknown-linux-gnu-x-x86_64-w64-mingw32

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

supported_targets() {
  find "$TARGETS_DIR" -maxdepth 1 -type f -name '*.sh' -print | \
    sed 's#.*/##' | sed 's/\.sh$//' | sort
}

expand_target_aliases() {
  local expanded=()
  local target
  local candidate

  for target in "$@"; do
    if [ "$target" = "all" ]; then
      while IFS= read -r candidate; do
        [ -n "$candidate" ] || continue
        expanded+=("$candidate")
      done < <(supported_targets)
    else
      expanded+=("$target")
    fi
  done

  TARGETS=("${expanded[@]}")
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

docker_host_target_for_target() {
  local target="$1"
  local host_target

  if is_linux_host_target "$target"; then
    printf '%s\n' "$target"
    return 0
  fi

  host_target="$(host_target_for_cross_target "$target" 2>/dev/null)" || return 1
  if is_linux_host_target "$host_target"; then
    printf '%s\n' "$host_target"
    return 0
  fi

  return 1
}

is_docker_target() {
  docker_host_target_for_target "$1" >/dev/null 2>&1
}

docker_platform_for_target() {
  local docker_host_target

  docker_host_target="$(docker_host_target_for_target "$1" 2>/dev/null)" || \
    die "unsupported Docker Linux host target: $1"

  case "$docker_host_target" in
    x86_64-unknown-linux-gnu)
      printf '%s\n' "linux/amd64"
      ;;
    aarch64-unknown-linux-gnu)
      printf '%s\n' "linux/arm64"
      ;;
    *)
      die "unsupported Docker Linux host target: $docker_host_target"
      ;;
  esac
}

ensure_ocaml_nested_submodule() {
  local submodule_name="$1"
  local marker_path="$2"
  local submodule_dir="$VENDORED_OCAML_DIR/$submodule_name"

  if [ -e "$submodule_dir/$marker_path" ]; then
    return 0
  fi

  echo "Initializing vendor/ocaml submodule $submodule_name"
  run_cmd git -C "$VENDORED_OCAML_DIR" submodule update --init "$submodule_name"

  if [ "$DRY_RUN" = "0" ] && [ ! -e "$submodule_dir/$marker_path" ]; then
    die "failed to initialize vendor/ocaml submodule $submodule_name"
  fi
}

ensure_required_submodules_for_target() {
  case "$1" in
    *-w64-mingw32)
      ensure_ocaml_nested_submodule "flexdll" "Makefile"
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

toolchain_suffix_name() {
  local suffix="${RIOT_TOOLCHAIN_SUFFIX:-}"
  suffix="${suffix#-}"
  printf '%s\n' "$suffix"
}

find_built_tarball() {
  local output_dir="$1"
  local suffix
  local pattern

  [ -d "$output_dir" ] || return 0

  suffix="$(toolchain_suffix_name)"
  if [ -n "$suffix" ]; then
    pattern="ocaml-*-${suffix}-*.tar.gz"
  else
    pattern="ocaml-*.tar.gz"
  fi

  find "$output_dir" -maxdepth 1 -type f -name "$pattern" | head -n 1
}

remove_built_tarballs_for_suffix() {
  local output_dir="$1"
  local suffix
  local pattern

  suffix="$(toolchain_suffix_name)"
  if [ -n "$suffix" ]; then
    pattern="ocaml-*-${suffix}-*.tar.gz"
  else
    pattern="ocaml-*.tar.gz"
  fi

  run_cmd find "$output_dir" -maxdepth 1 -type f \( -name "$pattern" -o -name "$pattern.sha256" \) -delete
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
  local tarball_path
  local checksum_path
  local needs_single_job_build=0

  platform="$(docker_platform_for_target "$target")"
  image_tag="riot-ocaml-toolchain:${target}"
  worktree_dir="$DOCKER_CACHE_ROOT/$target/worktree"

  command -v docker >/dev/null 2>&1 || die "docker is required for Linux host targets"
  docker buildx version >/dev/null 2>&1 || die "docker buildx is required for Linux host targets"
  ensure_required_submodules_for_target "$target"

  if [ "$CLEAN_BUILD" != "0" ]; then
    run_cmd rm -rf "$worktree_dir"
  fi

  run_cmd mkdir -p "$worktree_dir"
  run_cmd mkdir -p "$output_dir"
  remove_built_tarballs_for_suffix "$output_dir"
  run_cmd docker buildx build \
    --platform "$platform" \
    --load \
    --tag "$image_tag" \
    --file "$DOCKERFILE" \
    "$REPO_ROOT"

  if [ "$platform" = "linux/amd64" ] && [ "$(uname -s)" = "Darwin" ]; then
    case "$(uname -m)" in
      arm64|aarch64)
        needs_single_job_build=1
        ;;
    esac
  fi

  if [ "$needs_single_job_build" = "1" ]; then
    run_cmd docker run \
      --rm \
      --platform "$platform" \
      --env RIOT_TOOLCHAIN_SUFFIX="$RIOT_TOOLCHAIN_SUFFIX" \
      --env OCAML_TOOLCHAIN_SUFFIX="$RIOT_TOOLCHAIN_SUFFIX" \
      --env RIOT_OCAML_BUILD_JOBS=1 \
      --volume "$REPO_ROOT:/src:ro" \
      --volume "$worktree_dir:/work" \
      --volume "$output_dir:/out" \
      "$image_tag" \
      "$target" \
      "$CLEAN_BUILD"
  else
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
  fi

  if [ "$DRY_RUN" != "0" ]; then
    echo "dry-run: artifact path will be determined after packaging"
    return 0
  fi

  tarball_path="$(find_built_tarball "$output_dir")"
  [ -n "$tarball_path" ] || die "docker build did not produce a tarball for $target matching suffix $(toolchain_suffix_name)"
  checksum_path="$tarball_path.sha256"
  write_sha256_file "$tarball_path" "$checksum_path"
}

sync_ocaml_source_tree() {
  local source_dir="$1"
  local worktree_dir="$2"
  local files_to_copy
  local files_to_remove

  mkdir -p "$worktree_dir"

  files_to_copy="$(mktemp)"
  files_to_remove="$(mktemp)"

  git -C "$source_dir" ls-files -z --cached --others --exclude-standard | \
    while IFS= read -r -d '' path; do
      [ -e "$source_dir/$path" ] || continue
      printf '%s\0' "$path"
    done > "$files_to_copy"

  if [ -s "$files_to_copy" ]; then
    rsync -a --from0 --files-from="$files_to_copy" "$source_dir"/ "$worktree_dir"/
  fi

  git -C "$source_dir" ls-files -z --deleted > "$files_to_remove"
  if [ -s "$files_to_remove" ]; then
    while IFS= read -r -d '' path; do
      rm -rf "$worktree_dir/$path"
    done < "$files_to_remove"
  fi

  rm -f "$files_to_copy" "$files_to_remove"

  sync_ocaml_nested_submodules "$source_dir" "$worktree_dir"
}

sync_ocaml_nested_submodules() {
  local source_dir="$1"
  local worktree_dir="$2"
  local gitmodules_file="$source_dir/.gitmodules"
  local submodule_path=""

  [ -f "$gitmodules_file" ] || return 0

  git -C "$source_dir" config --file .gitmodules --get-regexp '^submodule\..*\.path$' | \
    while read -r _ submodule_path; do
      [ -n "$submodule_path" ] || continue

      if [ -d "$source_dir/$submodule_path" ]; then
        mkdir -p "$worktree_dir/$submodule_path"
        rsync -a --delete --exclude '.git' \
          "$source_dir/$submodule_path"/ \
          "$worktree_dir/$submodule_path"/
      else
        rm -rf "$worktree_dir/$submodule_path"
      fi
    done
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

  host_tarball="$(find_built_tarball "$host_output_dir")"
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

  ensure_required_submodules_for_target "$target"
  run_cmd mkdir -p "$output_dir"
  remove_built_tarballs_for_suffix "$output_dir"

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

  tarball_path="$(find_built_tarball "$output_dir")"
  [ -n "$tarball_path" ] || die "no tarball found to publish for $target in $output_dir matching suffix $(toolchain_suffix_name)"

  checksum_path="$tarball_path.sha256"
  [ -f "$checksum_path" ] || die "checksum file missing for $tarball_path"

  upload_object "$tarball_path" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$tarball_path")")"
  echo "  published: ${PUBLIC_BASE_URL%/}/$(basename "$tarball_path")"

  upload_object "$checksum_path" "$(join_object_key "$BUCKET_PREFIX" "$(basename "$checksum_path")")"
  echo "  checksum: ${PUBLIC_BASE_URL%/}/$(basename "$checksum_path")"
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

  if [ "$DRY_RUN" = "0" ]; then
    aws "${aws_args[@]}" > "$output_path"
  fi
}

generate_remote_manifest_json() {
  local output_path="$1"
  local listing_path
  local targets_path

  listing_path="$(mktemp "/tmp/riot-ocaml-objects.XXXXXX")"
  targets_path="$(mktemp "/tmp/riot-ocaml-targets.XXXXXX")"

  list_bucket_objects_json "$listing_path"
  supported_targets > "$targets_path"

  printf '+'
  printf ' %q' python3 - "$listing_path" "$targets_path" "$output_path" "$PUBLIC_BASE_URL" "$BUCKET_PREFIX"
  printf '\n'

  if [ "$DRY_RUN" = "0" ]; then
    python3 - "$listing_path" "$targets_path" "$output_path" "$PUBLIC_BASE_URL" "$BUCKET_PREFIX" <<'PY'
import json
import sys
from datetime import datetime, timezone

listing_path, targets_path, output_path, public_base_url, bucket_prefix = sys.argv[1:]

with open(listing_path, "r", encoding="utf-8") as listing_file:
    listing = json.load(listing_file)

with open(targets_path, "r", encoding="utf-8") as targets_file:
    supported_targets = [line.strip() for line in targets_file if line.strip()]

supported_targets.sort(key=len, reverse=True)
contents = listing.get("Contents") or []

def parse_toolchain(entry):
    key = entry.get("Key", "")
    if not key.startswith(f"{bucket_prefix}/"):
        return None

    artifact = key.split("/", 1)[1]
    if not artifact.startswith("ocaml-") or not artifact.endswith(".tar.gz"):
        return None

    artifact_target = None
    for candidate in supported_targets:
        suffix = f"-{candidate}.tar.gz"
        if artifact.endswith(suffix):
            artifact_target = candidate
            break

    if artifact_target is None:
        return None

    version_end = len(artifact) - len(f"-{artifact_target}.tar.gz")
    version = artifact[len("ocaml-"):version_end]
    if not version:
        return None

    if "-x-" in artifact_target:
        host, target = artifact_target.split("-x-", 1)
        kind = "cross"
    else:
        host = artifact_target
        target = artifact_target
        kind = "native"

    return {
        "version": version,
        "host": host,
        "target": target,
        "artifact_target": artifact_target,
        "kind": kind,
        "artifact": artifact,
        "artifact_url": f"{public_base_url.rstrip('/')}/{artifact}",
        "checksum_url": f"{public_base_url.rstrip('/')}/{artifact}.sha256",
        "size_bytes": entry.get("Size"),
        "last_modified": entry.get("LastModified"),
    }

toolchains = []
seen_artifacts = set()
for entry in contents:
    parsed = parse_toolchain(entry)
    if parsed is None:
        continue
    artifact_name = parsed["artifact"]
    if artifact_name in seen_artifacts:
        continue
    seen_artifacts.add(artifact_name)
    toolchains.append(parsed)

toolchains.sort(key=lambda item: (item["version"], item["host"], item["target"], item["artifact_target"]))

hosts = {}
for toolchain in toolchains:
    hosts.setdefault(toolchain["host"], set()).add(toolchain["target"])

manifest = {
    "schema_version": 1,
    "generated_at": datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
    "base_url": public_base_url.rstrip("/"),
    "versions": sorted({toolchain["version"] for toolchain in toolchains}),
    "hosts": [
        {
            "host": host,
            "targets": sorted(targets),
        }
        for host, targets in sorted(hosts.items())
    ],
    "toolchains": toolchains,
}

with open(output_path, "w", encoding="utf-8") as output_file:
    json.dump(manifest, output_file, indent=2)
    output_file.write("\n")
PY
  fi

  rm -f "$listing_path" "$targets_path"
}

publish_remote_manifest() {
  local manifest_path

  manifest_path="$(mktemp "/tmp/riot-ocaml-manifest.XXXXXX")"
  generate_remote_manifest_json "$manifest_path"
  upload_object "$manifest_path" "$(join_object_key "$BUCKET_PREFIX" "manifest.json")"
  echo "  manifest: ${PUBLIC_BASE_URL%/}/manifest.json"
  rm -f "$manifest_path"
}

ensure_artifact_exists() {
  local target="$1"
  local output_dir="$2"
  local tarball_path
  local checksum_path

  tarball_path="$(find_built_tarball "$output_dir")"
  if [ -n "$tarball_path" ]; then
    checksum_path="$tarball_path.sha256"
    if [ ! -f "$checksum_path" ]; then
      echo "Checksum missing for $(basename "$tarball_path"); regenerating"
      write_sha256_file "$tarball_path" "$checksum_path"
    fi
    return 0
  fi

  echo "No packaged artifact found for suffix $(toolchain_suffix_name); building $target first"
  if is_docker_target "$target"; then
    build_linux_host_target "$target" "$output_dir"
  else
    build_local_target "$target" "$output_dir"
  fi
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
    --suffix)
      shift
      [ $# -gt 0 ] || die "--suffix requires a value"
      CLI_TOOLCHAIN_SUFFIX="$1"
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
      if { [ "$MODE" = "publish" ] || [ "$MODE" = "release" ]; } && [ ${#TARGETS[@]} -eq 0 ] && [ -z "$CLI_TOOLCHAIN_SUFFIX" ] && [ ! -f "$TARGETS_DIR/$1.sh" ]; then
        CLI_TOOLCHAIN_SUFFIX="$1"
      else
        TARGETS+=("$1")
      fi
      ;;
  esac
  shift
done

[ -d "$VENDORED_OCAML_DIR" ] || die "vendored OCaml source not found at $VENDORED_OCAML_DIR"
[ "${#TARGETS[@]}" -gt 0 ] || die "at least one target is required"

load_env_file "$ENV_FILE"

RIOT_TOOLCHAIN_SUFFIX="${CLI_TOOLCHAIN_SUFFIX:-${INITIAL_RIOT_TOOLCHAIN_SUFFIX:-${RIOT_TOOLCHAIN_SUFFIX:-${INITIAL_OCAML_TOOLCHAIN_SUFFIX:-${OCAML_TOOLCHAIN_SUFFIX:-riot.3}}}}}"

if { [ "$MODE" = "publish" ] || [ "$MODE" = "release" ]; } && [ -z "$CLI_TOOLCHAIN_SUFFIX" ]; then
  if [ "${#TARGETS[@]}" -gt 1 ]; then
    if [ ! -f "$TARGETS_DIR/${TARGETS[0]}.sh" ]; then
      CLI_TOOLCHAIN_SUFFIX="${TARGETS[0]}"
      TARGETS=("${TARGETS[@]:1}")
      RIOT_TOOLCHAIN_SUFFIX="$CLI_TOOLCHAIN_SUFFIX"
    fi
  fi
fi

expand_target_aliases "${TARGETS[@]}"
[ "${#TARGETS[@]}" -gt 0 ] || die "at least one target is required"

if [ "$MODE" = "publish" ] || [ "$MODE" = "release" ]; then
  ensure_publish_env
fi

run_cmd mkdir -p "$OUTPUT_ROOT"
run_cmd mkdir -p "$DOCKER_CACHE_ROOT"
run_cmd mkdir -p "$LOCAL_CACHE_ROOT"

for target in "${TARGETS[@]}"; do
  [ -f "$TARGETS_DIR/$target.sh" ] || die "unknown target: $target"

  target_output_dir="$OUTPUT_ROOT/$target"
  artifact_present_before_publish="$(find_built_tarball "$target_output_dir" || true)"

  echo "======================================"
  echo " OCaml Toolchain Pipeline"
  echo "======================================"
  echo " Mode: $MODE"
  echo " Suffix: $RIOT_TOOLCHAIN_SUFFIX"
  echo " Target: $target"
  echo " Output: $target_output_dir"
  echo "======================================"
  echo

  if [ "$MODE" = "build" ] || [ "$MODE" = "release" ]; then
    if is_docker_target "$target"; then
      build_linux_host_target "$target" "$target_output_dir"
    else
      build_local_target "$target" "$target_output_dir"
    fi
  fi

  if [ "$MODE" = "publish" ] || [ "$MODE" = "release" ]; then
    ensure_artifact_exists "$target" "$target_output_dir"
    if [ "$DRY_RUN" != "0" ] && [ -z "$artifact_present_before_publish" ]; then
      echo "dry-run: publish step skipped until artifact exists"
    else
      publish_target "$target" "$target_output_dir"
    fi
  fi

  echo
done

if { [ "$MODE" = "publish" ] || [ "$MODE" = "release" ]; } && [ "$DRY_RUN" = "0" ]; then
  echo "======================================"
  echo " Publishing Toolchain Manifest"
  echo "======================================"
  publish_remote_manifest
  echo
fi
