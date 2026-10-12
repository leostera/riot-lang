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

The separately tested C-first bootstrap still emits deterministic C, builds a
C-hosted compiler, and runs its smoke/negative-input checks with Node excluded
from `PATH`. The owned native Codegen path remains a selected-function harness;
whole-module/global initialization and the native self-rebuild are not
implemented.
