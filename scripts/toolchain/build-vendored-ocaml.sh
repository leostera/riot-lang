#!/bin/sh

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/../.." && pwd)
source_dir="$repo_root/vendor/ocaml"
prefix="$source_dir/compiler"

usage() {
  cat <<EOF
Usage: $0 [--prefix PATH]

Build and install the vendored OCaml compiler from vendor/ocaml.

Options:
  --prefix PATH   Install prefix to use. Defaults to vendor/ocaml/compiler
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
    --prefix)
      shift
      [ $# -gt 0 ] || {
        echo "error: --prefix requires a path" >&2
        exit 1
      }
      prefix="$1"
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "error: unknown argument: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
  shift
done

if [ ! -f "$source_dir/configure" ]; then
  echo "error: vendor/ocaml is missing; initialize the submodule first" >&2
  exit 1
fi

jobs=$(getconf _NPROCESSORS_ONLN 2>/dev/null || true)
if [ -z "$jobs" ]; then
  jobs=$(sysctl -n hw.ncpu 2>/dev/null || echo 4)
fi

mkdir -p "$prefix"

echo "==> Building vendored OCaml"
echo "    source: $source_dir"
echo "    prefix: $prefix"

cd "$source_dir"
./configure \
  --prefix="$prefix" \
  --with-relative-libdir=../lib/ocaml \
  --enable-runtime-search \
  --enable-runtime-search-target=fallback
make -j"$jobs"
make install
