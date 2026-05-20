#!/usr/bin/env bash
set -euo pipefail

env LLVM_SYS_221_PREFIX=/opt/homebrew/opt/llvm cargo check --manifest-path stage0/Cargo.toml >/tmp/stage0-check.log 2>&1 || {
  tail -80 /tmp/stage0-check.log >&2
  exit 1
}

cargo test --manifest-path rt/Cargo.toml -- --test-threads=1 >/tmp/rt-test.log 2>&1 || {
  tail -80 /tmp/rt-test.log >&2
  exit 1
}
