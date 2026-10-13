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

## Earlier C-backend baseline at `db25954a4`

This was the primary baseline before the ABI-free AArch64 MachineIR inspection
stage was added (`db25954a4bc3c75b112826761ceb3efce0d570d8`). The worktree was
clean. Since `f419986a2`, the compiler source graph had grown to include
TypedAST, LambdaIR, NativeIR and its verifier; the C emitter and inference
implementation had also changed. The runtime C/header fingerprints were
unchanged. Do not interpret the historical and earlier timing delta as an
isolated codegen regression. The updated primary baseline is below.

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

## Earlier C-backend baseline at `929d8b7ee`

This was the primary reproducible baseline after adding the ABI-free scalar
AArch64 MachineIR selector, printer and inspection command
(`929d8b7eeff755c19b29b480191ab4c4786a6c97`). The worktree was clean. The
measured compile path remains the existing C-plus-Clang reference; the
MachineIR CLI is not invoked by this benchmark. Its source is nevertheless
part of the compiler graph and contributes to Stage 0 and native compiler
output. The NativeIR/C-path separation, runtime C/header contracts, and
language behavior are otherwise unchanged from `db25954a4`. This series is
superseded by the updated primary baseline below.

| Stage (seconds) | Sample 1 | Sample 2 | Sample 3 | Median |
|---|---:|---:|---:|---:|
| Stage 0 -> JS | 0.913 | 0.365 | 0.367 | 0.367 |
| JS-hosted compiler -> native compiler | 122.204 | 117.924 | 120.279 | 120.279 |
| Standalone Clang of generated C | 2.153 | 2.138 | 2.184 | 2.153 |
| Native compiler self-rebuild | 294.708 | 295.981 | 296.598 | 295.981 |
| Successor compiles the smoke program | 0.597 | 0.569 | 0.577 | 0.577 |

Artifacts were stable across all three samples: generated C 4,081,362 bytes
(first build) and 4,081,358 bytes (self-rebuild); compiler and successor
executables 2,024,856 bytes; smoke C 874 bytes; smoke executable 55,784 bytes.
The root Stage 0 JavaScript module was 26,506 bytes (dependency modules are
emitted separately). All smoke programs exited 42. Ill-typed input returned 1
and left neither an executable nor a `.c` file. The Stage 0 Rust build, excluded
from sample timings, took 7.377 seconds in this run.

The source graph and compiler binary are larger than at `db25954a4` because
MachineIR selection/printing and CLI inspection code is now compiled as part of
the compiler. The timing changes between these revisions are not a controlled
measurement of the C code generator or the owned backend. No numeric acceptance
threshold has been selected.

Reproduce from a clean checkout:

```sh
git worktree add --detach ../riot-lang-worktrees/aarch64-mir-c-baseline 929d8b7ee
python3 tools/benchmarks/riotc_c_bootstrap.py \
  --root ../riot-lang-worktrees/aarch64-mir-c-baseline --iterations 3
```

Raw logs and `results.json` for this series were retained at
`/var/folders/qy/3zl3lsrj0s9fwtjgjs6dx6vr0000gn/T/riotc-c-bootstrap-baseline-kyselmk2`
on the measurement host.

## Updated C-backend baseline at `fcd131c20`

This is the primary reproducible baseline at the source revision after scalar
AArch64 selection gained boolean operations and `i32`/`bool` literal-pattern
tests (`fcd131c2058dda3c498cd64a6c0314545b6d72e9`). The worktree was clean.
The measured compile path remains the existing C-plus-Clang reference; the
MachineIR inspection command is not invoked by this benchmark. Its source is
nevertheless part of the compiler graph and contributes to Stage 0 and native
compiler output. The NativeIR/C-path separation, runtime C/header contracts,
and language behavior are otherwise unchanged from `db25954a4`.

| Stage (seconds) | Sample 1 | Sample 2 | Sample 3 | Median |
|---|---:|---:|---:|---:|
| Stage 0 -> JS | 0.917 | 0.375 | 0.369 | 0.375 |
| JS-hosted compiler -> native compiler | 121.098 | 123.792 | 126.447 | 123.792 |
| Standalone Clang of generated C | 2.157 | 2.135 | 2.169 | 2.157 |
| Native compiler self-rebuild | 296.020 | 295.183 | 295.630 | 295.630 |
| Successor compiles the smoke program | 0.612 | 0.597 | 0.600 | 0.600 |

Artifacts were stable across all three samples: generated C 4,099,640 bytes
(first build) and 4,099,636 bytes (self-rebuild); compiler and successor
executables 2,041,592 bytes; smoke C 872 bytes; smoke executable 55,784 bytes.
The root Stage 0 JavaScript module was 26,506 bytes (dependency modules are
emitted separately). All smoke programs exited 42. Ill-typed input returned 1
and left neither an executable nor a `.c` file. The Stage 0 Rust build, excluded
from sample timings, took 7.471 seconds in this run.

The selected C path remains independent of AArch64 selection and still emits C
for the compiler source. The source and compiler outputs grow compared with
`929d8b7ee` because of the additional scalar/literal-pattern selector code.
The timings are not a controlled measurement of the C code generator or owned
backend. No numeric acceptance threshold has been selected.

Reproduce from a clean checkout:

```sh
git worktree add --detach ../riot-lang-worktrees/aarch64-scalar-c-baseline fcd131c20
python3 tools/benchmarks/riotc_c_bootstrap.py \
  --root ../riot-lang-worktrees/aarch64-scalar-c-baseline --iterations 3
```

Raw logs and `results.json` for this series were retained at
`/var/folders/qy/3zl3lsrj0s9fwtjgjs6dx6vr0000gn/T/riotc-c-bootstrap-baseline-vovnoveh`
on the measurement host.

## C-path/NativeIR separation finding

At `d05e5927f`, compiling `riotc/main.ml` still lowered and verified LambdaIR
and NativeIR before selecting the C backend. One self-rebuild attempt was
terminated by signal 9 after 419.739 seconds, before it wrote successor C or an
executable. `db25954a4` keeps the C differential path independent of those
owned-IR checks; the `native-ir` inspection command continues to perform them.
The three successful samples at `db25954a4` above confirm native self-rebuild
and the status-42/ill-typed checks after this separation.

This command split is not a generated-C fallback for an owned backend. The C
path remains the existing behavioral reference while NativeIR and the
ABI-free AArch64 MachineIR slice are inspected separately. ABI lowering,
LinearIR and Codegen were unimplemented at this historical revision, pending
an approved target/runtime boundary.

## Current C-backend baseline at `84a823a86` (single sample)

A clean worktree at `84a823a862249f4c73c7cf9a9aec1b673c1b45d1` was measured on
2026-10-13 using the same script, host, and tools as the previous series. This
remains the **legacy C-plus-Clang source-backend baseline**: it uses the
Stage 0 -> JavaScript -> JS-hosted compiler route for the measurements below.
It is separate from the new C-first Stage 0 bridge and no-JavaScript acceptance
chain. This is one sequential sample, not a controlled comparison or a
performance threshold.

| Stage (seconds) | Sample 1 |
|---|---:|
| Stage 0 -> JS | 2.099 |
| JS-hosted compiler -> native compiler | 189.264 |
| Standalone Clang of generated C | 3.401 |
| Native compiler self-rebuild | 787.611 |
| Successor compiles the smoke program | 0.872 |

The Stage 0 Rust build, excluded from these sample timings, took 11.967 s.
Artifacts: Stage 0 JavaScript 81,871 bytes; generated C 5,898,406 bytes for
both first build and self-rebuild; native compiler and successor 2,842,888
bytes each; smoke C 923 bytes; smoke executable 56,392 bytes. The smoke exited
42; ill-typed input returned 1 and left neither an executable nor `.c` file.
This later compiler source is substantially larger than the historical C
baseline. The single sample must not be interpreted as a controlled regression
or as evidence about the owned backend's eventual performance.

The benchmark JSON and per-command logs are retained at
`/var/folders/qy/3zl3lsrj0s9fwtjgjs6dx6vr0000gn/T/riotc-c-bootstrap-baseline-lwivpvh7`.
Reproduce this legacy-baseline measurement from a clean checkout of the
recorded commit with:

```sh
git worktree add --detach ../riot-lang-worktrees/owned-closure-c-baseline-84a823a86 84a823a86
python3 tools/benchmarks/riotc_c_bootstrap.py \
  --root ../riot-lang-worktrees/owned-closure-c-baseline-84a823a86 --iterations 1
```

The script intentionally invokes the historical JavaScript-hosted path for
this baseline. It is not the C-first/no-JavaScript acceptance command.
