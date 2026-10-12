# Riotc C + Clang bootstrap baseline (2026-10-12)

This records repeatable measurements for the C-source backend that remains the
behavioral/performance reference while the owned backend is developed. It is
not a release-performance threshold.

## Method

`tools/benchmarks/riotc_c_bootstrap.py` runs sequential samples from a clean
checkout. It builds the Rust Stage 0 binary once (that build is outside the
sample timings), then measures Stage 0 -> JavaScript, JS-hosted C generation and
Clang, a native compiler self-rebuild, and a separate Clang build of the first
generated C file. It also compiles/runs a status-42 program with the successor
and verifies an ill-typed program fails without leaving either an executable
or generated C. Raw per-command logs and `results.json` are written to a fresh
temporary output directory and retained for inspection.

The measurements use `time.perf_counter()` around subprocess wall time. They
are sequential, not CPU-isolated; file-system caches and host load are not
controlled. Reported summaries are medians of three samples, with individual
samples below. The first Stage 0 run in each series was slower, so the full
samples are included rather than treating the median as a universal constant.

Host: macOS 26.6.2, Apple Silicon `arm64`, 12 logical CPUs. Tools: Cargo/rustc
1.99.0, Node 26.11.0, Python 3.14.8, Apple Clang 21.0.0. Clang printed a
non-fatal missing Homebrew MariaDB search-path warning during linking.

The benchmark JSON also records the exact repository revision and Git blob
fingerprints for `riotc/main.ml`, `riotc/codegen/c/emit.ml`, `riotc/runtime.c`,
and `riotc/runtime.h`, in addition to the host/tool versions.

## Historical C baseline at `f419986a2`

This is the compiler source revision used for the original baseline
(`f419986a22ebe2e1773b2b769553f3081782c84d`). Its source tree was clean. The C
emitter/runtime fingerprints were recorded by the benchmark script; the C
emitter has since changed, so this row is historical context, not the primary
comparison point for future work.

| Stage (seconds) | Sample 1 | Sample 2 | Sample 3 | Median |
|---|---:|---:|---:|---:|
| Stage 0 -> JS | 0.567 | 0.193 | 0.189 | 0.193 |
| JS-hosted compiler -> native compiler | 53.023 | 52.527 | 52.754 | 52.754 |
| Standalone Clang of generated C | 1.028 | 1.019 | 1.029 | 1.028 |
| Native compiler self-rebuild | 105.327 | 103.969 | 103.383 | 103.969 |
| Successor compiles the smoke program | 0.577 | 0.426 | 0.577 | 0.577 |

Artifacts were stable across all three samples: generated C 2,054,079 bytes
(first build) and 2,054,075 bytes (self-rebuild); compiler and successor
executables 1,097,368 bytes; smoke C 907 bytes; smoke executable 55,784 bytes.
The root Stage 0 JavaScript module was 14,581 bytes (dependency modules are
emitted separately). All smoke programs exited 42. Ill-typed input returned 1
and left neither an executable nor a `.c` file.

Reproduce from the repository containing this benchmark script:

```sh
git worktree add --detach ../riot-lang-worktrees/baseline-f419986a2 f419986a2
python3 tools/benchmarks/riotc_c_bootstrap.py \
  --root ../riot-lang-worktrees/baseline-f419986a2 --iterations 3
```

## Current C-backend baseline at `db25954a4`

This is the primary baseline for the current compiler source before the
ABI-dependent ARM backend (`db25954a4bc3c75b112826761ceb3efce0d570d8`). The
worktree was clean. Since `f419986a2`, the compiler source graph has grown to
include TypedAST, LambdaIR, NativeIR and its verifier; the C emitter and
inference implementation have also changed. The runtime C/header fingerprints
are unchanged. Do not interpret the historical and current timing delta as an
isolated codegen regression.

| Stage (seconds) | Sample 1 | Sample 2 | Sample 3 | Median |
|---|---:|---:|---:|---:|
| Stage 0 -> JS | 0.720 | 0.732 | 0.354 | 0.720 |
| JS-hosted compiler -> native compiler | 120.233 | 120.218 | 120.376 | 120.233 |
| Standalone Clang of generated C | 2.013 | 1.988 | 2.010 | 2.010 |
| Native compiler self-rebuild | 290.388 | 289.588 | 297.540 | 290.388 |
| Successor compiles the smoke program | 0.645 | 0.581 | 0.522 | 0.581 |

Artifacts were stable across all three samples: generated C 3,877,167 bytes
(first build) and 3,877,163 bytes (self-rebuild); compiler and successor
executables 1,920,744 bytes; smoke C 910 bytes; smoke executable 55,784 bytes.
The root Stage 0 JavaScript module was 22,199 bytes (dependency modules are
emitted separately). All smoke programs exited 42. Ill-typed input returned 1
and left neither an executable nor a `.c` file.

Reproduce from a clean checkout:

```sh
git worktree add --detach ../riot-lang-worktrees/current-c-independent db25954a4
python3 tools/benchmarks/riotc_c_bootstrap.py \
  --root ../riot-lang-worktrees/current-c-independent --iterations 3
```

The C-backend self-rebuild remains much slower than the historical revision,
which now contains substantially more compiler source and checker/IR logic.
No numeric acceptance threshold has been selected.

## C-path/NativeIR separation finding

At `d05e5927f`, compiling `riotc/main.ml` still lowered and verified LambdaIR
and NativeIR before selecting the C backend. One self-rebuild attempt was
terminated by signal 9 after 419.739 seconds, before it wrote successor C or an
executable. `db25954a4` keeps the C differential path independent of those
owned-IR checks; the `native-ir` inspection command continues to perform them.
The three successful current-baseline samples above confirm native self-rebuild
and the status-42/ill-typed checks after this separation.

This command split is not a generated-C fallback for an owned backend. The C
path remains the existing behavioral reference while the NativeIR path is
inspected separately and the ARM pipeline is not yet implemented.
