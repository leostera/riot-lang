# AArch64 native backend slice

`machine_ir.ml`, `select_machine_ir.ml`, and `print_machine_ir.ml` define an
ABI-free virtual-register inspection stage. Run it through the Stage 0-hosted
compiler with:

```sh
riotml aarch64-mir SOURCE TOP_LEVEL_FUNCTION
```

The command lowers and verifies the source graph as NativeIR, then selects only
the requested top-level function and prints its AArch64 MachineIR. It does not
claim that the whole source module is ready for native code generation.
Unsupported instructions return a diagnostic with function, block, and source
span; there is no C fallback from this command.

The selected subset currently includes typed `unit`, `i32`, and `bool` values
(`unit` remains visible in MachineIR but has no physical value); `i32`
addition, subtraction, multiplication, negation, signed comparisons, and
boolean equality/inequality; unit/i32/bool literal-pattern tests; conditional
and unconditional branches with block arguments; returns, unreachable and
trap pseudos; plus direct or Host calls whose arguments and result are in this
value subset. Calls retain their logical target, typed virtual arguments, and
NativeIR effects, but do not assign physical argument/result registers or name
runtime symbols.

Division/remainder are rejected until their explicit zero-trap behavior is
lowered. Closure creation/calls/captures, strings, aggregates, globals, and
switches are outside this selection slice. A64 virtual registers preserve
NativeIR SSA IDs, and labels preserve NativeIR block IDs. No physical
registers, stack/frame layout, object format, assembler, runtime-value layout,
or external calling convention is selected by MachineIR. In particular,
`A64Trap` remains a semantic pseudo until a concrete trap sequence is selected
and tested.

## LinearIR

`lower_linear_ir.ml`, `linear_ir.ml`, and `verify_linear_ir.ml` add an initial
ABI-lowered, linearly ordered IR. Inspect it with:

```sh
riotml aarch64-linear-ir SOURCE TOP_LEVEL_FUNCTION
```

The current lowerer handles the selected scalar/control-flow slice using a
simple all-spill strategy: each non-unit SSA value receives a distinct
unboxed four-byte stack slot, unit is erased, local frames are rounded up to
16 bytes, and `w9`/`w10`/`w11` are the caller-saved scratch registers. Function
entry scalar arguments arrive in `w1`; direct top-level/module-initializer
calls set the hidden environment `x0` to null, pass an optional scalar in
`w1`, and take scalar results from `w0`. Unit arguments/results have no data
transfer. Conditional edges are explicit labels with typed spill/reload copies.

The LinearIR verifier checks value/slot ownership, unique non-overlapping
slots, stack-frame bounds/alignment, the physical-register assignment,
terminated labeled blocks, and branch targets. Nested direct calls that need a
closure environment and Host helpers requiring `RtValue` marshalling are
rejected before Codegen. Pointer/composite values and narrow integer widths
are not yet selected for this LinearIR.

## Codegen status

AArch64 assembly Codegen, the external `main` adapter, trap sequences, and
assembler/linker invocation are not implemented yet. LinearIR's `mov_imm`,
`cmp_set`, logical direct-call target, and trap forms are inspectable pseudos,
not assembler input. The owned pipeline remains incomplete; unsupported
RiotML operations must be rejected before Codegen, never emitted as C. The
existing C-plus-Clang source backend remains a separate measured baseline.

The approved macOS AArch64 target, complete intended value classes, internal
function convention, and C runtime/Host helper boundary are recorded in
[`ABI.md`](ABI.md). The current C-first Stage 0 bridge is separate from both
MachineIR and LinearIR.

Run the executable tests for the selection and LinearIR fixtures with:

```sh
cargo test -p tinyml --test cli_tests aarch64_machine_ir
cargo test -p tinyml --test cli_tests aarch64_linear_ir_abi_and_control_flow_fixtures_execute
```
