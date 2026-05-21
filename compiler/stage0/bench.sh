#!/usr/bin/env sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
STAGE0_MANIFEST="$ROOT/stage0/Cargo.toml"
FIXTURE_DIR="$ROOT/fixtures/programs/basic"
LLVM_PREFIX=${LLVM_SYS_221_PREFIX:-/opt/homebrew/opt/llvm}

export LLVM_SYS_221_PREFIX="$LLVM_PREFIX"

python3 - "$STAGE0_MANIFEST" "$FIXTURE_DIR" <<'PY'
import subprocess
import sys
import time

manifest = sys.argv[1]
fixture_dir = sys.argv[2]
work = [
    ("typed", f"{fixture_dir}/compiler_like_token_classifier.ml"),
    ("ir", f"{fixture_dir}/lambda_variant_renderer.ml"),
    ("llvm", f"{fixture_dir}/compiler_like_ast_fold.ml"),
]

start = time.perf_counter()
for pass_name, path in work:
    subprocess.run(
        ["cargo", "run", "--quiet", "--manifest-path", manifest, "--", "emit", pass_name, path],
        check=True,
        stdout=subprocess.DEVNULL,
    )
elapsed_ms = (time.perf_counter() - start) * 1000.0
print(f"METRIC total_ms={elapsed_ms:.3f}")
print(f"benchmarked {len(work)} stage0 emit passes")
PY
