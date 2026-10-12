# Riotc owned-native pipeline progress (2026-10-12)

This is a development probe, not a native self-rebuild or acceptance result. It
is intentionally separate from the C-plus-Clang source-backend baseline in
[`riotc-c-bootstrap-baseline-2026-10-12.md`](riotc-c-bootstrap-baseline-2026-10-12.md).

## C-first compiler and owned-pipeline probe

On `aarch64-apple-darwin`, using the Rust TinyML Stage 0 compiler and Clang
21.0.0, Stage 0 emitted `riotc/main.ml`'s C-hosted compiler and Clang built it
with `-O2`:

- Stage 0 generated C: **5,866,349 bytes**.
- Clang-built C-hosted compiler: **1,749,360 bytes**.
- With `PATH=/usr/bin:/bin` and `RIOTML_RUNTIME_DIR="$PWD/riotc"`, invoking
  `aarch64-mir riotc/main.ml main` took **303 seconds** (integer-second shell
  timing) and exited 1.
- It stopped before Codegen at `riotc/main.ml::main`, source codepoints
  `26424..26465`: the nested constructor/string pattern used to dispatch on
  `Host.args` is outside the current AArch64 selector subset.

The C-hosted compiler was built and could execute the CLI, but this command did
not emit native code and is not evidence of the required self-rebuild chain.
The 303-second figure includes full import-graph checking and the root-only
LambdaIR/NativeIR lowering path; it is not comparable to the baseline
compiler's JS-to-native or self-rebuild measurements. At the time of this
probe the failure used the generic selector-subset diagnostic. A follow-up
adds the specific `nested constructor patterns require explicit runtime
matching` diagnostic and tests that rejection leaves no executable or
assembly; pattern lowering itself remains unimplemented. Clang emitted a
benign warning about the missing optional MariaDB connector search path during
link.

## Follow-up direct-call-graph slice

A separate macOS/AArch64 integration test now builds the Stage 0 C-hosted
compiler with Clang `-O2`, then invokes
`aarch64-compile-direct-graph` with `PATH=/usr/bin:/bin` and
`RIOTML_RUNTIME_DIR` set. The compiler emitted a two-function AArch64 program and a self-recursive
countdown program through reachable monomorphic same-module direct-call graphs;
both native programs exited 42. The same no-Node path rejected ill-typed input
before Codegen and removed stale executable/assembly outputs. The direct-graph
command checks that the module initializer is one no-parameter block with a
return, containing only zero-capture closure creation, global stores, and unit
constants; it skips that initializer and selected-function global loads fail
in selection. Non-closure initializer work is rejected. This is not a
whole-module compiler or a native self-rebuild.

The separately tested C-first bootstrap still emits deterministic C, builds a
C-hosted compiler, and runs its legacy `compile`/smoke/negative-input path with
Node excluded from `PATH`. The C backend and direct-call native graph are
distinct CLI paths.

A clean `git archive` of commit `17d6e9d51` was also built on
`aarch64-apple-darwin25.6.0` with Apple Clang 21.0.0, keeping
`PATH=/usr/bin:/bin` throughout the Stage 0, Clang, C-hosted, and native
commands. Stage 0 C emission was deterministic; the C-hosted C-backend smoke
and owned direct-call-graph smoke both exited 42, and ill-typed input was
rejected with stale outputs removed. The artifacts were: generated compiler C
**5,984,852 bytes**, optimized C-hosted compiler **1,783,616 bytes**, and
native direct-graph fixture **56,464 bytes**. This is a bounded clean-checkout
C-first/native slice, not a performance comparison to the baseline and not the
required native Riotc compiler or successor self-rebuild. The full
compiler-source corpus remains unimplemented.

## Clean-archive scalar ABI extension

A clean archive of `043f06c2cd` was built on Apple Silicon (`arm64`, macOS
26.6.2, Apple Clang 21.0.0). The Rust Stage 0 compiler emitted deterministic
C twice; Clang built the C-hosted compiler. With `PATH=/usr/bin:/bin` and
`RIOTML_RUNTIME_DIR` set to the archive's `riotc/`, the C-hosted compiler
compiled and ran the status-42 C-backend smoke and an owned native direct-call
graph exercising explicitly ascribed `i64` literals, typed `i64` arguments and
results, and `i64` block-edge copies; both exited 42. The same restricted-PATH
run rejected an ill-typed source before Codegen and left neither executable nor
C output. Generated C was **6,030,980 bytes**, and the C-hosted compiler was
**1,800,128 bytes**. This validates only the bounded scalar ABI slice and
C-first bridge, not whole-module Codegen, native self-rebuild, or a comparable
performance measurement. Clang printed the existing optional MariaDB library
search-path warning during linking.

## Same-fixture smoke microprobe

Using the C-hosted compiler built from that clean archive, I compiled the same
`let main = fn () -> 42` fixture five times through each available path, with
`PATH=/usr/bin:/bin` and the runtime directory fixed:

- Existing C-plus-Clang source backend: median **129.50 ms**, generated C
  **854 bytes**, executable **56,248 bytes**.
- Owned AArch64 direct-graph path: median **164.15 ms**, executable
  **56,400 bytes**.

All ten produced executables exited 42. The latest recorded C-backend
baseline at `fcd131c20` reports this status-42 smoke at 0.600 s, 872 bytes of C,
and 55,784 bytes for the executable. The current C backend generated 854 bytes of C (18 bytes less) and a smoke
executable 464 bytes larger; the owned direct-graph executable is 616 bytes
larger. These are warm local measurements including compiler process startup
and Clang linking. The C-hosted compiler was built with `-O2`, unlike the
historical baseline compiler, so timing differences are not a controlled
comparison. The small fixture and different compiler revision do not establish
performance of the full owned compiler or satisfy the final benchmark gate; no
threshold is asserted.
