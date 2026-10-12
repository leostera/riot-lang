# AArch64 MachineIR slice

`machine_ir.ml`, `select_machine_ir.ml`, and `print_machine_ir.ml` define an
ABI-free virtual-register inspection stage. Run it through the Stage 0-hosted
compiler with:

```sh
riotml aarch64-mir SOURCE TOP_LEVEL_FUNCTION
```

The command first lowers and verifies the source graph as NativeIR, then selects
only the requested top-level function and prints its AArch64 MachineIR. It does
not claim that the whole source module is ready for native code generation.
Unsupported instructions return a diagnostic with function, block, and source
span; there is no C fallback from this command.

The selected subset currently includes `i32`/`bool` constants; `i32`
addition, subtraction, multiplication, negation, signed comparisons, and boolean
equality/inequality; `i32`/`bool` literal-pattern tests; conditional/unconditional
branches, block arguments, returns, unreachable and trap pseudos; plus direct or
Host calls whose arguments and result are scalar. Calls retain their logical
target, typed virtual arguments, and NativeIR effects, but do not assign
physical argument/result registers or name runtime symbols.

Division/remainder are rejected until their explicit zero-trap behavior is
lowered. Closure calls, closure capture, strings, unit values, aggregates,
globals, and switches are outside this slice. A64 virtual registers preserve
NativeIR SSA IDs, and labels preserve NativeIR block IDs. No physical
registers, stack/frame layout, object format, assembler, runtime-value layout,
or external calling convention is selected by this MachineIR slice. In
particular, `A64Trap` remains a semantic pseudo until a concrete trap sequence
is selected and tested.

Run the executable scalar, branch, direct/Host-call, effect-preservation,
unmodeled-effect rejection and unsupported-division/remainder fixtures with:

```sh
cargo test -p tinyml --test cli_tests aarch64_machine_ir
```

The approved macOS AArch64 target, native value classes, internal function
convention, and C runtime/Host helper boundary are recorded in
[`ABI.md`](ABI.md). MachineIR selection intentionally remains virtual-register
only; it does not itself assign physical registers, emit calls, or implement
ABI lowering.

This is a partial MachineIR stage, not LinearIR or Codegen.
