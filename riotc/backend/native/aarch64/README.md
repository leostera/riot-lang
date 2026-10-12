# AArch64 native backend slice

`machine_ir.ml`, `select_machine_ir.ml`, and `print_machine_ir.ml` define an
ABI-free virtual-register inspection stage. Run it through the Stage 0-hosted
compiler with:

```sh
riotml aarch64-mir SOURCE TOP_LEVEL_FUNCTION
```

The command typechecks the complete import graph, then Lambda-lowers and
lowers/verifies only the root module as NativeIR, using dependency global
signatures from checked module interfaces. It selects only the requested
top-level function and prints its AArch64 MachineIR; it does not lower imported
modules to LambdaIR/NativeIR or claim that the whole source module is ready for
native code generation.
Unsupported instructions return a diagnostic with function, block, and source
span; there is no C fallback from this command.

The selected subset currently includes typed `unit`, `i32`, and `bool` values
(`unit` remains visible in MachineIR but has no physical value), signed `i64`
values, and opaque pointer values for concrete strings, composites, and
function values. `i64` values support explicitly ascribed decimal literals
within the checked `i64` range, parameters, returns, direct-call
arguments/results, and block-edge copies; arithmetic, comparisons, and pattern
tests remain unsupported. Pointer values can be passed through a function unchanged; string
constants and operations that inspect or construct pointed-to values remain
unsupported. The slice also includes `i32` addition, subtraction,
multiplication, negation, signed comparisons, and boolean equality/inequality;
typed i32 division/remainder helper calls; unit/i32/bool literal-pattern tests;
conditional and unconditional branches with block arguments; returns,
unreachable and trap pseudos; plus direct calls and the explicit
`tinyml/lib/host.ml::args` Host call. Calls retain typed virtual arguments and
NativeIR effects. Runtime division/remainder use explicit helper IDs with
fixed `i32, i32 -> i32` signatures; `Host.args` uses the typed opaque-pointer
`rt_native_host_args` bridge. Other calls remain logical until ABI lowering.

Closure creation/calls/captures, string constants, constructor/tuple/record
pattern tests, aggregate construction or projection, globals, and switches
are outside this selection slice. Unsupported pattern shapes report that
runtime representation lowering is required rather than entering Codegen. A64
virtual registers preserve NativeIR SSA IDs, and labels preserve NativeIR
block IDs. No physical registers, stack/frame layout, object format, assembler,
runtime-value layout, or external calling convention is selected by MachineIR. `A64Trap` remains a semantic pseudo until
a concrete trap sequence is selected and tested.

## LinearIR

`lower_linear_ir.ml`, `linear_ir.ml`, and `verify_linear_ir.ml` add an initial
ABI-lowered, linearly ordered IR. Inspect it with:

```sh
riotml aarch64-linear-ir SOURCE TOP_LEVEL_FUNCTION
```

The current lowerer handles the selected scalar/pointer/control-flow slice
using a simple all-spill strategy: each non-unit SSA value receives a distinct
slot (`i32`/`bool`: four bytes; `i64`/opaque pointer: eight bytes aligned to
eight), unit is erased, local frames are rounded up to 16 bytes, and
`w9`/`w10`/`w11` plus `x9` are the caller-saved scratch registers. Function
entry `i32`/`bool` arguments arrive in `w1`; `i64`/pointer arguments arrive in
`x1`. Direct top-level/module-initializer calls set the hidden environment
`x0` to null, pass an optional `i32`/`bool` in `w1` or `i64`/pointer in `x1`,
and take results from `w0` or `x0`. Typed runtime division/remainder use
their declared C helper ABI: `i32` arguments in `w0`/`w1` and an `i32` result in
`w0`, without a hidden environment. Unit arguments/results have no data
transfer. Conditional edges are explicit labels with typed spill/reload copies.

The LinearIR verifier checks value/slot ownership, unique non-overlapping
slots, stack-frame bounds/alignment, the physical-register assignment,
terminated labeled blocks, and branch targets. Nested direct calls that need a
closure environment and Host helpers without an explicit typed bridge are
rejected before Codegen. `Host.args` erases its unit argument and returns an
opaque pointer in `x0`; concrete composite layouts/operations and `i8`/`i16`/
unsigned integer widths are not yet supported by this LinearIR.

## Codegen

`emit.ml` emits textual AArch64 Mach-O assembly from verified LinearIR. It
lowers spill loads/stores, i32 arithmetic/comparisons, i64 pass-through values
and unary negation, branches, direct calls with symbols in the supplied
function set, frame
prologues/epilogues, and trap pseudos. Block labels are function-qualified. Typed i32
division/remainder call `_rt_native_i32_div`/`_rt_native_i32_rem`, whose exact
prototypes are in `runtime.h`; the linker includes the existing `runtime.c`.
`Host.args` calls `_rt_native_host_args`, whose declared `void *` helper keeps
`RtValue` conversion inside the runtime. Pattern-failure traps call the
no-argument helper `rt_pattern_fail` and retain a `brk` fallback if it returns.
Other `NativeTrapKind`s currently become distinct `brk` immediates. Host helpers
beyond `Host.args` and general `RtValue` marshalling are not implemented.

Exercise the current function-level path with:

```sh
riotml aarch64-compile-function SOURCE TOP_LEVEL_FUNCTION -o OUTPUT
```

It writes inspectable assembly to `OUTPUT.aarch64.s`, then invokes Clang for
`arm64-apple-macosx26.0` to assemble and link `OUTPUT` with the discovered
`riotc/runtime.c` implementation. The synthetic external
`_main` adapter accepts only an erased unit/no source argument and an `i32`
result; it passes incoming `argc`/`argv` to `rt_set_args`, sets the hidden
environment to null, calls the selected Riot function, and returns its status.
Existing output is removed before the attempt, and a failed Clang invocation
removes any partial executable.

This command is deliberately a **single-function Codegen harness**, not a
whole-module native compiler. It selects and verifies only the named function;
it does not emit module initializers, globals, closure creation, general
runtime/Host helper marshalling, or other functions. Direct calls fail unless
their target is present in the Codegen function set (currently one function
for this CLI).

`aarch64-compile-direct-graph SOURCE ENTRY -o OUTPUT` expands the Codegen set
to the entry's reachable same-module `NativeDirectCall` targets. It accepts
only a single-block, no-parameter initializer with a return terminator and
instructions limited to zero-capture closure creation, global stores, and unit
constants; it verifies and skips that initializer. Selected functions that
load globals still fail in selection. It does not include unrelated functions,
imported closure targets, or general module initialization, so it is not a
whole-module compiler.

Codegen currently rejects frames larger than 16,380 bytes, the largest
all-spill frame supported by its immediate addressing sequence.

The harness is useful for exercising assembly and linking but does not prove
that module initialization or the complete compiler source graph works. Those
unsupported constructs must remain explicit failures in a future whole-module
command, never C fallback. The existing C-plus-Clang source backend remains a
separate measured baseline.

The approved macOS AArch64 target, complete intended value classes, internal
function convention, and C runtime/Host helper boundary are recorded in
[`ABI.md`](ABI.md). The current C-first Stage 0 bridge is separate from both
MachineIR and LinearIR.

Run the executable tests for selection, LinearIR, and the AArch64
Codegen/link harness with:

```sh
cargo test -p tinyml --test cli_tests aarch64_machine_ir
cargo test -p tinyml --test cli_tests aarch64_linear_ir_abi_and_control_flow_fixtures_execute
cargo test -p tinyml --test cli_tests aarch64_codegen_assembles_links_and_removes_failed_outputs
cargo test -p tinyml --test cli_tests c_hosted_bootstrap_compiles_native_direct_call_graph_without_javascript
```

That Codegen test compares process status, stdout, and stderr against the
existing C backend for scalar arithmetic, signed comparisons/branches, wrapped
integer operations, i32 division/remainder (including zero and `INT32_MIN / -1`
edge behavior), two-function and recursive direct-call graphs, typed i64 call
and block-edge pass-through, pattern-failure diagnostics,
and a `Host.args` helper call. It also confirms unsupported Host helpers,
nested constructor patterns, and non-closure global initialization fail before
Codegen. A separate macOS/AArch64 test builds the C-first C-hosted compiler
with Node excluded and uses it to compile/run direct-call and recursive
native graphs. These are bounded slices, not evidence that Riotc itself or unsupported
string/Host/global semantics work through whole-module native Codegen.
