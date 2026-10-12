# RiotML compiler bootstrap source

`riotc/` contains the RiotML compiler implementation in bootstrap-compatible
TinyML/RiotML source. The required bootstrap is C-first: Rust TinyML Stage 0
emits C for this source graph, then Clang builds a C-hosted compiler. The
commands below use no JavaScript or Node.

Stage 0's C output is only a bootstrap bridge. The compiler's existing
C-plus-Clang source backend remains a separate, measured behavioral baseline;
it is not a fallback in the owned native backend. That owned path currently
reaches TypedAST, LambdaIR, target-independent SSA NativeIR, ABI-independent
AArch64 virtual-register MachineIR, an initial all-spill AArch64 LinearIR, and
a tested AArch64 assembly/link function harness. Whole-module Codegen,
runtime/Host helper marshalling, and the final native self-rebuild acceptance
chain are not complete.

## Verified C-first Stage 0 bridge

Run from the repository root. Build TinyML Stage 0, emit C for the complete
Riotc source graph, and build the C-hosted compiler:

```sh
cargo build -p tinyml
work="$(mktemp -d)"
PATH=/usr/bin:/bin target/debug/tinyml compile --target c -I riotc \
  riotc/main.ml -o "$work/riotml-compiler.c"
PATH=/usr/bin:/bin target/debug/tinyml compile --target c -I riotc \
  riotc/main.ml -o "$work/riotml-compiler-again.c"
cmp "$work/riotml-compiler.c" "$work/riotml-compiler-again.c"
PATH=/usr/bin:/bin clang -std=gnu11 -I riotc \
  "$work/riotml-compiler.c" riotc/runtime.c \
  -o "$work/riotml-compiler"
```

Then exercise the C-hosted compiler with Node excluded from `PATH`:

```sh
export RIOTML_RUNTIME_DIR="$PWD/riotc"
printf 'let main = fn () -> 42\n' > "$work/smoke.ml"
PATH=/usr/bin:/bin "$work/riotml-compiler" compile \
  "$work/smoke.ml" -o "$work/smoke"
PATH=/usr/bin:/bin "$work/smoke"
smoke_status=$?
test "$smoke_status" -eq 42

printf 'let main = fn () -> 1 + true\n' > "$work/invalid.ml"
rm -f "$work/invalid" "$work/invalid.c"
if PATH=/usr/bin:/bin "$work/riotml-compiler" compile \
    "$work/invalid.ml" -o "$work/invalid"; then
  echo 'ill-typed input unexpectedly compiled' >&2
  exit 1
fi
test ! -e "$work/invalid"
test ! -e "$work/invalid.c"
```

The restricted `PATH` contains Clang and POSIX tools but not Node. The
C-hosted compiler's current `compile` command still uses Riotc's C-plus-Clang
source backend. This proves the Stage 0 bridge and its failure boundary; it
does not yet prove compilation through the owned AArch64 backend. The final
acceptance chain must additionally build an owned native compiler, self-rebuild
it, and have the successor compile and run the status-42 fixture.

## Compiler pipeline status

The checker loads the complete reachable source-module graph and performs
rank-1 Hindley–Milner inference, preserving polymorphic schemes and nominal
type identities across module boundaries. Local `let` bindings and imported
values are independently instantiated; constructors and patterns are checked
against their payload and nominal types. The native entry point must have type
`unit -> i32`.

TypedAST retains resolved binding identities, checked types, and source spans.
LambdaIR is a typed, target-independent desugared core. NativeIR is typed
target-independent SSA with explicit blocks, terminators, block arguments, and
call/allocation/trap/Host effects; it has a deterministic printer, verifier,
and positive/negative tests. Same-module, monomorphic top-level function
references become direct-call candidates by binding identity during NativeIR
lowering; aliases, imported values, and generalized functions retain closure
semantics. The `native-ir SOURCE` command prints this IR.
`aarch64-mir SOURCE TOP_LEVEL_FUNCTION` inspects the current ABI-free scalar
and opaque-pointer AArch64 MachineIR selection. `aarch64-linear-ir SOURCE
TOP_LEVEL_FUNCTION` lowers and verifies the unit/i32/bool ABI slice plus
opaque-pointer pass-through, with explicit spill slots, call-register
assignments, and linearized control flow.
`aarch64-compile-function SOURCE TOP_LEVEL_FUNCTION -o OUTPUT` emits and links
AArch64 assembly for one selected function and writes `OUTPUT.aarch64.s` for
inspection. This is a function-level Codegen harness, not a whole-module
compiler: it omits module initializers and globals, closure construction,
runtime/Host helper marshalling, and unselected functions. A direct-call target
must be in the supplied Codegen set, so the one-function CLI explicitly rejects
calls to other functions. Unsupported operations in the selected function fail
before Codegen rather than being emitted as C. The full compiler must similarly reject unsupported module
constructs; it must not use this harness as a fallback.

The existing source compiler still goes through `codegen/c/emit.ml` and
Clang. Its supported subset includes functions and closures, local bindings,
applications, `i32`, booleans, strings, tuples, records, named variants, and
their supported patterns and matches. This subset is narrower than Stage 0
TinyML's documented type/operator surface. Unsupported C-backend constructs
fail before output is written; this baseline is separate from the unfinished
owned native path.

`Host` and the bundled `String` module are connected to the C runtime. The
runtime preserves embedded NUL bytes in string values, file contents, and
stdout/stderr; OS paths and process names/arguments reject NUL explicitly.
Text-file I/O uses strict UTF-8 validation. Clang is launched through
`Host.run_process`; runtime discovery uses `RIOTML_RUNTIME_DIR`, the current
working directory, and the source/runtime directories. The existing
integration tests exercise Host files, arguments, directories, processes,
errors, output, and exit status. The `compile_c_first_bootstraps_riotc_without_javascript`
integration test emits the complete Riotc graph twice, checks deterministic C,
builds the C-hosted compiler, runs the NUL-string status-42 smoke with Node
excluded from `PATH`, and checks that an ill-typed program leaves no executable
or C output.

The existing C-plus-Clang source-backend and self-rebuild baseline is recorded
in [`../docs/benchmarks/riotc-c-bootstrap-baseline-2026-10-12.md`](../docs/benchmarks/riotc-c-bootstrap-baseline-2026-10-12.md).
Those measurements are distinct from the Rust TinyML Stage 0 to C bridge.

## Source organization

- `syntax/ast.ml` and `syntax/parser/`: source types, lexer, and parser.
- `check/names.ml` and `check/inference.ml`: name checks, HM inference,
  module interfaces, nominal constructor schemes, and native-entry validation.
- `modules.ml`: dependency-first module graph loading, missing-module
  reporting, and cycle detection.
- `lambda/` and `native_ir/`: typed core lowering, SSA lowering, printing, and
  verification.
- `backend/native/aarch64/`: virtual-register MachineIR, initial ABI lowering,
  verified LinearIR, assembly Codegen and a single-function Clang link harness;
  whole-module Codegen and runtime/Host marshalling remain future work.
- `codegen/c/emit.ml`: existing C-plus-Clang source-backend baseline.
- `runtime.h`, `runtime.c`: tagged values, closures, module environments, and
  the C-hosted runtime linked by Clang.
- `main.ml`: compiler pipeline, runtime discovery, and CLI.

Run `cargo fmt --all -- --check` and `cargo test --workspace` for the current
Rust Stage 0 and compiler regression suites.
