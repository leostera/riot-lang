#!/bin/bash

set -euo pipefail

TARGET="${1:-}"
CLEAN_BUILD="${2:-0}"
SOURCE_DIR="/src/vendor/ocaml"
WORK_DIR="/work/vendor/ocaml"
OUTPUT_DIR="/out"
WORKTREE_LAYOUT_VERSION="3"
VERSION_FILE="$WORK_DIR/.riot-worktree-layout-version"

toolchain_suffix_name() {
  local suffix="${RIOT_TOOLCHAIN_SUFFIX:-${OCAML_TOOLCHAIN_SUFFIX:-}}"
  suffix="${suffix#-}"
  printf '%s\n' "$suffix"
}

find_built_tarball() {
  local output_dir="$1"
  local suffix
  local pattern

  suffix="$(toolchain_suffix_name)"
  if [ -n "$suffix" ]; then
    pattern="ocaml-*-${suffix}-*.tar.gz"
  else
    pattern="ocaml-*.tar.gz"
  fi

  find "$output_dir" -maxdepth 1 -type f -name "$pattern" | head -n 1
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

linux_sdk_arch_dir() {
  case "$1" in
    x86_64-unknown-linux-gnu)
      printf '%s\n' "x86_64-linux-gnu"
      ;;
    aarch64-unknown-linux-gnu)
      printf '%s\n' "aarch64-linux-gnu"
      ;;
    *)
      return 1
      ;;
  esac
}

install_linux_sdk_overlay() {
  local target="$1"
  local target_dir="$2"
  local lib_dir
  local sysroot_dir

  lib_dir="$(linux_sdk_arch_dir "$target")" || return 0
  sysroot_dir="$target_dir/sysroot"

  echo "Installing Linux SDK overlay for $target into $sysroot_dir"

  rm -rf "$sysroot_dir"
  mkdir -p "$sysroot_dir/usr/include" "$sysroot_dir/usr/lib"

  cp -a /usr/include/uuid "$sysroot_dir/usr/include/"
  cp -a /usr/include/openssl "$sysroot_dir/usr/include/"
  cp -a /usr/include/pcre2.h "$sysroot_dir/usr/include/"
  [ ! -f /usr/include/pcre2posix.h ] || cp -a /usr/include/pcre2posix.h "$sysroot_dir/usr/include/"
  cp -a /usr/include/zlib.h "$sysroot_dir/usr/include/"
  cp -a /usr/include/zconf.h "$sysroot_dir/usr/include/"
  cp -a "/usr/include/$lib_dir" "$sysroot_dir/usr/include/"
  [ ! -d "/usr/include/$lib_dir/openssl" ] || cp -a "/usr/include/$lib_dir/openssl/." "$sysroot_dir/usr/include/openssl/"

  (
    shopt -s nullglob
    for pattern in libuuid.so* libuuid.a libssl.so* libssl.a libcrypto.so* libcrypto.a libpcre2-8.so* libpcre2-8.a libz.so* libz.a; do
      for file in /usr/lib/"$lib_dir"/$pattern /lib/"$lib_dir"/$pattern; do
        [ -e "$file" ] || continue
        cp -a "$file" "$sysroot_dir/usr/lib/"
      done
    done
  )

  if [ "$target" = "x86_64-unknown-linux-gnu" ]; then
    (cd "$sysroot_dir/usr" && ln -sf lib lib64)
  fi
}

die() {
  echo "error: $*" >&2
  exit 1
}

[ -n "$TARGET" ] || die "target is required"
[ -d "$SOURCE_DIR" ] || die "vendored OCaml source not mounted at $SOURCE_DIR"

mkdir -p /work "$OUTPUT_DIR"

if [ "$CLEAN_BUILD" != "0" ]; then
  rm -rf "$WORK_DIR"
elif [ ! -f "$VERSION_FILE" ] || [ "$(cat "$VERSION_FILE" 2>/dev/null || true)" != "$WORKTREE_LAYOUT_VERSION" ]; then
  echo "Resetting cached OCaml worktree at $WORK_DIR"
  rm -rf "$WORK_DIR"
fi

mkdir -p "$WORK_DIR"

# Sync tracked and untracked source files into the cached worktree without
# copying ignored build outputs. That keeps the Linux cache incremental while
# avoiding Mach-O and other host-specific artefacts from the source checkout.
sync_ocaml_source_tree "$SOURCE_DIR" "$WORK_DIR"

printf '%s\n' "$WORKTREE_LAYOUT_VERSION" > "$VERSION_FILE"

cd "$WORK_DIR"
bootstrapped_host=0
host_target="$(host_target_for_cross_target "$TARGET" 2>/dev/null || true)"
if [ -n "$host_target" ] && [ ! -d "$WORK_DIR/cross/$host_target" ]; then
  echo "Bootstrapping host toolchain for $TARGET inside Docker worktree"
  bash ./cross/build.sh "$host_target"
  bootstrapped_host=1
fi

if [ "$bootstrapped_host" != "0" ]; then
  echo "Resetting source tree after host bootstrap for $TARGET"
  make distclean
fi

bash ./cross/build.sh "$TARGET"
install_linux_sdk_overlay "$TARGET" "$WORK_DIR/cross/$TARGET"
bash ./cross/package.sh "$TARGET" "$OUTPUT_DIR"

tarball="$(find_built_tarball "$OUTPUT_DIR")"
[ -n "$tarball" ] || die "package step did not produce a tarball for $TARGET"

(cd "$OUTPUT_DIR" && sha256sum "$(basename "$tarball")" > "$(basename "$tarball").sha256")

bash ./cross/test-relocatable.sh "$TARGET"
