#!/bin/sh

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
ZORT_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
OUT_DIR=${OUT_DIR:-"$ZORT_DIR/zig-out/e2e-ml-zort"}
OCAMLOPT=${OCAMLOPT:-"$HOME/.riot/toolchains/5.5.0-riot.2/aarch64-apple-darwin/bin/ocamlopt.opt"}
CC=${CC:-cc}
if [ "$#" -gt 0 ]; then
  ZORT_COMPAT_DYLIB=$1
else
  ZORT_COMPAT_DYLIB=${ZORT_COMPAT_DYLIB:-"$ZORT_DIR/zig-out/lib/libzort-compiler-compat.dylib"}
fi

if [ ! -x "$OCAMLOPT" ]; then
  echo "missing ocamlopt.opt at $OCAMLOPT" >&2
  exit 1
fi

if [ ! -f "$ZORT_COMPAT_DYLIB" ]; then
  echo "missing zort compiler compat dylib at $ZORT_COMPAT_DYLIB" >&2
  exit 1
fi

mkdir -p "$OUT_DIR"

compile_case() {
  case_name=$1
  ml_source=$2
  host_stub=$3

  case_dir="$OUT_DIR/$case_name"
  mkdir -p "$case_dir"

  ml_basename=$(basename "$ml_source")
  ml_copy="$case_dir/$ml_basename"
  ml_object="$case_dir/$case_name.o"
  exe="$case_dir/$case_name.zort"

  cp "$SCRIPT_DIR/ml/$ml_source" "$ml_copy"
  "$OCAMLOPT" -nostdlib -nopervasives -output-obj -o "$ml_object" "$ml_copy"

  $CC \
    "$host_stub" \
    "$ml_object" \
    "$ZORT_COMPAT_DYLIB" \
    -Wl,-rpath,"$(dirname "$ZORT_COMPAT_DYLIB")" \
    -lm \
    -o "$exe"

  "$exe" >"$case_dir/stdout.txt"
  printf "e2e-ml-zort %s ok %s\n" "$case_name" "$(cat "$case_dir/stdout.txt")"
}

compile_case \
  "min_external_startup" \
  "min_external_startup.ml" \
  "$SCRIPT_DIR/ml/min_external_startup_main.c"
