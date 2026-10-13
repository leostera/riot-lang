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

## C-first C-backend self-rebuild probe

From the same clean archive of `043f06c2cd` on Apple Silicon/macOS 26.6.2
with Apple Clang 21.0.0, the Stage 0 C-hosted compiler compiled `riotc/main.ml`
through the existing Riotc C-plus-Clang source backend. With `PATH=/usr/bin:/bin`
(and no `node` executable on that path), this source-backend self-rebuild took
**286.96 s real** (**276.04 s user**, **10.43 s system**). It emitted
**5,186,508 bytes** of C and a **2,528,136-byte** executable. Repeated C output
was byte-identical. The rebuilt successor compiled and ran the status-42 smoke;
it also rejected the ill-typed fixture without leaving an executable or C
output. Clang again printed the optional MariaDB search-path warning.

This is a C-first, no-Node check of the existing source-backend baseline. It is
not the required owned-native compiler build/rebuild chain, and its timing and
artifact sizes are not directly comparable to the historical JS-to-native
baseline or the Stage 0 TinyML-to-C bridge.

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

## 2026-10-13 entry-specific compiler-source attempt

From the current working tree, Rust TinyML emitted C for `riotc/main.ml` and
Clang built the C-hosted compiler with `PATH=/usr/bin:/bin`; neither step
invoked Node. Generated C was **6,340,590 bytes** and the C-hosted compiler
**1,870,248 bytes**. The build took 6 seconds in the integer-second shell
timing. The no-Node C-hosted compiler then ran
`aarch64-compile-direct-graph riotc/main.ml main`; after 362 seconds it exited
1 before Codegen with `closure creation is outside the AArch64 selector` in
`riotc/main.ml::run_clang` (source codepoints `3230..3576`). No native executable
or assembly remained. Entry-specific NativeIR lowering now follows only
reachable same-module direct-call targets and conservatively refuses
non-function module initializers; this probe still reaches curried function
closures and does not demonstrate native compiler generation or self-rebuild.
The timing includes full import checking and this entry graph lowering and is
not comparable to the baseline measurements or a controlled performance
comparison.

The later selector diagnostic identifies this as
`closure creation requires a concrete native closure layout`; the AArch64
Codegen test asserts that closure creation fails before leaving executable or
assembly artifacts.

## 2026-10-13 C-hosted i64 runtime-helper smoke

The focused `c_hosted_bootstrap_compiles_native_direct_call_graph_without_javascript`
integration test builds Stage 0 C and the C-hosted compiler with
`PATH=/usr/bin:/bin`, then compiles and runs a direct-call graph exercising
signed i64 division and remainder through `rt_native_i64_div` and
`rt_native_i64_rem`. The executable exits 42; Node is absent from the commands
used by this test. The AArch64 Codegen test also checks i64 divide-by-zero
trapping and `INT64_MIN / -1` plus `INT64_MIN % -1` wrap behavior. The existing
C backend rejects these i64 operations, so this is not a C/native behavioral
differential. These are working-tree tests, not a clean-archive native compiler
or self-rebuild, and no compile-size or timing comparison is claimed.

## 2026-10-13 clean-archive i64 graph slice

A `git archive` of commit `9f379b7fd` was extracted to a fresh temporary
folder. The existing Rust Stage 0 binary emitted C for the archived
`riotc/main.ml`, and Clang built its C-hosted compiler with `PATH=/usr/bin:/bin`
(no Node invocation). The generated C was **6,442,070 bytes**, the C-hosted
compiler **1,905,376 bytes**, and the status-42 i64 division/remainder and
equality direct-graph executable **56,904 bytes**. An ill-typed program was
rejected with status 1 and neither its stale executable nor assembly remained.
This is a clean-source-archive, bounded no-JavaScript bootstrap/native feature
check, but Stage 0 itself was the existing local Rust binary; it does not prove
that the native compiler or its successor can be generated.

## 2026-10-13 clean-archive native compiler-source attempt

Using the C-hosted compiler built from that archive, with `PATH=/usr/bin:/bin`,
`aarch64-compile-direct-graph riotc/main.ml main` took **368 seconds** and
exited 1 before Codegen. The specific diagnostic was
`closure creation requires a concrete native closure layout` in
`riotc/main.ml::run_clang` (source codepoints `3309..3655`). Stale executable
and assembly outputs were removed. This reproduces the closure-layout blocker
from archived sources with Node excluded; it is not native compiler generation,
a self-rebuild, or a comparable performance measurement.

## 2026-10-13 owned closure-capture slice

The C-hosted compiler now lowers closure creation and capture loads through
Riot-owned MachineIR, LinearIR, and AArch64 Codegen. The record stores an
8-byte code pointer followed by naturally aligned typed captures, uses
`rt_native_alloc(size_t)` only for raw storage, copies capture values so the
closure can outlive its creator, and passes the record pointer as hidden `x0`
on indirect calls. LinearIR verifies capture layout, hidden-environment spill,
runtime allocation, and closure-call register setup. The reachable code graph
includes functions referenced by closure creation as well as direct calls.

On `aarch64-apple-darwin`, the focused C-hosted no-JavaScript integration test
passes: Stage 0 emits C, Clang builds the compiler, and with
`PATH=/usr/bin:/bin` that compiler produces a native program where `make 40`
returns a capturing closure and the caller invokes it with `2`; the executable
exits 42. The same source compiled through Riotc's separate C backend also
exits 42 with matching stdout/stderr. A direct probe confirmed the emitted
assembly allocates a 16-byte record, stores the code pointer at offset 0 and
captured `i32` at offset 8, reloads the closure code/environment, and executes
`blr x16`. The closure is returned across the creator's stack frame, so this
also exercises the copy-based escape-safety policy. This is still a bounded
feature test, not whole-module Codegen or native self-rebuild.

With closure graph reachability in place, the next C-hosted compiler-source
attempt (`aarch64-compile-direct-graph riotc/main.ml main`, `PATH=/usr/bin:/bin`)
no longer stops at the former closure-layout diagnostic. It now fails before
Codegen while selecting `nested(riotc/main.ml::clang_arguments@3002,3061,3061)`:
`string constants are outside the scalar selector`. This establishes progress
past the previous closure blocker, but the owned compiler still cannot build
itself; concrete strings and additional compiler-source operations remain
unsupported. The attempt is not a self-rebuild or a benchmark comparison.

After integrating the closure lowering, verifier fixtures, and no-Node escaping
closure test, `cargo test --workspace` passed on the working tree (75 library
unit tests, 2 CLI unit tests, 21 checker tests, 59 CLI integration tests, 50
corpus tests, 1 module test, and 1 pipeline API test). The same full suite also
passed from the clean worktree at `84a823a86`. `cargo fmt --all -- --check` and
`git diff --check` passed on the active source tree. The CLI suite includes
existing Node-dependent tests; the acceptance-specific Stage 0 → C-hosted
compiler → native closure test separately runs with `PATH=/usr/bin:/bin`. That test also
confirms a string literal and tuple construction, still unsupported by the
owned selector, fail before Codegen; it also confirms an unsupported
`Host.current_directory` call is rejected during LinearIR lowering. Each case
removes stale executable/assembly artifacts. The no-Node test invokes
`native-ir` and checks closure creation/capture-load metadata, then exercises a
nested escaping closure that
captures an `i32` at offset 8 and an `i64` at offset 16; the executable returns
42. It also exercises a zero-capture closure with an 8-byte code-pointer-only
record, and a closure capturing a zero-sized `unit` plus a function pointer;
NativeIR preserves both captures and the native record stores the pointer at
offset 8 in a 16-byte allocation.

## 2026-10-13 clean-archive closure slice

A clean `git archive` of `2214839a8` on arm64 macOS 26.6.2 with Apple Clang
21.0.0 was built with `PATH=/usr/bin:/bin`. The existing local Rust Stage 0
compiler emitted **6,866,267 bytes** of C, and Clang built a **2,009,808-byte**
C-hosted compiler in 7 seconds of integer-second shell timing. With Node excluded,
that compiler compiled a fixture where `make 40` returns an escaping captured
closure and the caller invokes it with `2`; the native executable exited 42
(**56,704 bytes**, with **1,748 bytes** of inspectable AArch64 assembly).

The no-Node `c_hosted_bootstrap_compiles_native_direct_call_graph_without_javascript`
integration regression also passed from the clean worktree at `84a823a86`. It builds
Stage 0 and the C-hosted compiler from that checkout, inspects NativeIR closure
capture metadata, executes direct/recursive/zero-capture/mixed-width escaping
closure programs, and checks ill-typed/string/aggregate/unsupported-Host
rejection plus output cleanup. This verifies only the bounded smoke surface,
not full native compiler generation.

The same clean-archive compiler then attempted
`aarch64-compile-direct-graph riotc/main.ml main`. It exited 1 before Codegen
while selecting `nested(riotc/main.ml::clang_arguments@3002,3061,3061)` with
`string constants are outside the scalar selector` at source codepoints
`3085..3097`. No native executable or assembly remained. This is reproducible
clean-source-archive evidence for the closure slice and explicit unsupported
string failure only: Stage 0 is the existing local Rust binary, and neither
native compiler generation nor successor self-rebuild has succeeded. These
sizes and the 7-second C-host bootstrap are not a controlled comparison to the
existing C-plus-Clang full compiler baseline.

A bounded same-fixture comparison used a C-hosted compiler built from the clean
archive at `2214839a8`, with `PATH=/usr/bin:/bin`. The escaping-closure fixture
was compiled three times through the existing C backend and three times through
the owned AArch64 graph command; all six executables exited 42. Median compile
time was **0.124231 s** for C emission plus Clang (2,476 generated-C bytes,
56,504-byte executable) and **0.140665 s** for owned AArch64 selection plus
Clang (1,748-byte assembly, 56,704-byte executable). These sequential
microfixture samples are not controlled full-compiler measurements or an
acceptance threshold; the owned native compiler still cannot compile Riotc
itself.

For scale, a separate single-sample run of the legacy C-plus-Clang baseline at
clean commit `84a823a86` took 189.264 s for JS-hosted compiler-to-native, 3.401 s
for standalone Clang, and 787.611 s for the native C-backend self-rebuild; its
generated C was 5,898,406 bytes and the executable 2,842,888 bytes. That
benchmark intentionally uses the legacy JavaScript-hosted route and is not the
new C-first/no-JavaScript acceptance chain or a comparison against a completed
owned native compiler. Full host/tool details and logs are in
`riotc-c-bootstrap-baseline-2026-10-12.md`.