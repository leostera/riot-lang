#!/usr/bin/env sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OUT_DIR="$ROOT/target/riotc-smoke"
OUT="$OUT_DIR/riotc"

mkdir -p "$OUT_DIR"
LLVM_SYS_221_PREFIX=${LLVM_SYS_221_PREFIX:-/opt/homebrew/opt/llvm} \
  cargo run --quiet --manifest-path "$ROOT/stage0/Cargo.toml" -- \
  compile "$ROOT/riotc/src/main.ml" -o "$OUT"
"$OUT" "$@"
