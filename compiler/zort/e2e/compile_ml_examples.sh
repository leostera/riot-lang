#!/bin/sh

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
ZORT_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
OUT_DIR=${OUT_DIR:-"$ZORT_DIR/zig-out/e2e-ml"}
OCAMLOPT=${OCAMLOPT:-"$HOME/.riot/toolchains/5.5.0-riot.3/aarch64-apple-darwin/bin/ocamlopt.opt"}
CC=${CC:-cc}

if [ ! -x "$OCAMLOPT" ]; then
  echo "missing ocamlopt.opt at $OCAMLOPT" >&2
  exit 1
fi

OCAML_WHERE=$("$OCAMLOPT" -where)
NATIVE_C_LIBS=$("$OCAMLOPT" -config | sed -n 's/^native_c_libraries:[[:space:]]*//p')

mkdir -p "$OUT_DIR"

compile_case() {
  case_name=$1
  ml_source=$2
  host_stub=$3
  primitive_stub=${4:-}

  case_dir="$OUT_DIR/$case_name"
  mkdir -p "$case_dir"

  ml_basename=$(basename "$ml_source")
  ml_copy="$case_dir/$ml_basename"
  ml_object="$case_dir/$case_name.o"
  vendor_exe="$case_dir/$case_name.vendor"

  cp "$SCRIPT_DIR/ml/$ml_source" "$ml_copy"
  "$OCAMLOPT" -g -output-obj -o "$ml_object" "$ml_copy"

  link_args="$host_stub $ml_object"
  if [ -n "$primitive_stub" ]; then
    link_args="$link_args $primitive_stub"
  fi

  # `-output-obj` is the native compiler mode intended for embedding/linking
  # against a custom runtime. Today we link these fixtures against vendor
  # `libasmrun` as the baseline; the same emitted objects are the future input
  # to zort's compiler-compatibility shim.
  # shellcheck disable=SC2086
  $CC -I"$OCAML_WHERE" $link_args -L"$OCAML_WHERE" -lasmrun $NATIVE_C_LIBS -lm -o "$vendor_exe"

  "$vendor_exe" >"$case_dir/stdout.txt"
  printf "e2e-ml %s ok %s\n" "$case_name" "$(cat "$case_dir/stdout.txt")"
}

compile_case \
  "noalloc_callback" \
  "noalloc_callback.ml" \
  "$SCRIPT_DIR/ml/noalloc_callback_main.c"

compile_case \
  "alloc_pair_callback" \
  "alloc_pair_callback.ml" \
  "$SCRIPT_DIR/ml/alloc_pair_callback_main.c"

compile_case \
  "external_identity_callback" \
  "external_identity_callback.ml" \
  "$SCRIPT_DIR/ml/external_identity_callback_main.c" \
  "$SCRIPT_DIR/ml/external_identity_primitive.c"
