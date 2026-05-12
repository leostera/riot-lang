# Raml IR Requirements For Wasm

This document turns the pipeline and runtime findings into a concrete question:

what kind of IR stack does `raml` need if native, JS, and wasm are all real
backends?

The main point is simple:

the shared IR cannot be JS-shaped and it cannot be bytecode-shaped.

## 1. Why Melange's Custom IR Is Not A Shared `raml` IR

Melange proves that a Lambda-retargeting path can work well.

It does not prove that Melange's custom IR family should become `raml`'s shared
boundary.

Why not:

- `Lam_convert` already rewrites around JS-specific runtime behavior
- the compiler tracks JS runtime modules explicitly
- `.cmj` artifacts store delayed JS programs and JS package info
- `J.program` is a JavaScript IR by design

So Melange's IRs are useful evidence.
They are not backend-neutral.

## 2. Why Bytecode Is Also The Wrong Shared `raml` IR

`wasm_of_ocaml` proves that bytecode is a stable and practical backend input.

That is valuable.

But for `raml`, bytecode is still the wrong main shared boundary.

Why not:

- it centers the design on compatibility with the OCaml toolchain rather than
  on `raml`'s own backend needs
- it is too low-level for a compiler that also wants a native backend and a
  JS backend under one package
- it makes target-independent optimization and artifact design harder to own
  inside `raml`

Bytecode can still be a useful import path or compatibility path later.
It should not be the defining IR for a Riot-owned multi-backend compiler.

## 3. What The Shared IR Must Preserve

A real shared `Raml Core IR` needs to preserve at least the semantics that all
three backends care about.

### Module and unit structure

`Raml Core IR` needs explicit notions of:

- compilation unit identity
- module initialization order
- exported values
- imported units/modules
- side-effect-at-initialization facts

Melange's `.cmj` metadata and `wasm_of_ocaml`'s separate-compilation support
both show that this cannot stay implicit.

### Closures and calling information

`Raml Core IR` needs explicit notions of:

- closure creation
- arity
- known direct calls versus indirect closure calls
- tail calls
- partial application and curry policy

Native, JS, and wasm all need this information, even if they lower it
differently.

### Allocation and mutation categories

`Raml Core IR` needs to distinguish at least:

- immutable block allocation
- mutable block allocation
- bytes/string allocation
- boxed numeric allocation
- initialization writes
- mutation writes

This distinction matters for:

- GC/barrier policy
- JS runtime helpers
- Wasm GC representation
- `zort` mutator integration

### Control and effects

`Raml Core IR` needs first-class control constructs for:

- conditionals
- switches
- loops
- exception raise/catch
- effect perform
- effect resume
- effect reperform

If effects only appear as opaque externals, the backends lose too much control
too early.

### Primitive taxonomy

`Raml Core IR` should distinguish:

- runtime primitives
- host imports
- user foreign primitives
- pure arithmetic/logical operations

That keeps the backend honest about what it can inline, what it must import,
and what depends on a runtime package.

### Data-class facts

`Raml Core IR` needs stable value/data classes, for example:

- immediates/atoms
- tuples/blocks
- variants
- strings
- bytes
- boxed floats
- boxed `int32`
- boxed `int64`
- boxed `nativeint`
- JS-host values if the source language exposes them

This should stay above concrete JS or Wasm representation details.

### Artifact metadata

`Raml Core IR` also needs companion metadata for:

- exports
- purity or initialization effects
- inlining candidates
- dependency edges
- source spans/origins for diagnostics

Melange's `.cmj` and Dune's separate compilation controls both point in this
direction.

## 4. A Reasonable `raml` IR Stack

One plausible decomposition is:

### Frontend lowering IR

Owns:

- source-semantic normalization
- match lowering policy
- recursive-value lowering policy
- explicit origins/spans

This is the layer closest to the source language.

### Shared `Raml Core IR`

Owns:

- modules and unit init
- closures and calls
- allocation and mutation
- exceptions and effects
- target-neutral primitive taxonomy
- export/import metadata

This is the layer all backends should consume.

### Backend-specific IR families

Owns:

- JS object-model and module-system lowering
- Wasm GC/reference-types lowering and import model
- native ABI and target code generation

This is where target commitments become explicit.

### Artifact layer

Owns:

- per-backend summaries
- separate-compilation metadata
- runtime sidecar manifests
- loader and packager outputs

This keeps build products out of the semantic core IR itself.

## 5. What The Wasm Backend Needs From The Shared `Raml Core IR`

The wasm backend in particular needs the shared `Raml Core IR` to preserve enough data to
make these decisions late:

- which values stay immediates versus heap refs
- which allocations become Wasm GC arrays or structs
- which helper imports are required
- what the module initialization graph is
- which host interop hooks are needed
- whether effect lowering uses JSPI, CPS, or another strategy

If those decisions are already frozen into JS helpers or bytecode assumptions,
the shared `Raml Core IR` is too low-level or too target-shaped.

## 6. What The Shared `Raml Core IR` Must Not Do

Some anti-goals are just as important.

### Do not encode JS property access or JS module names

That belongs in a JS-specific lowering family.

### Do not encode Wasm GC opcodes or `(ref eq)` directly

That belongs in a Wasm-specific lowering family.

### Do not encode raw native ABI details

That belongs in a native-specific lowering family or compatibility layer.

### Do not collapse all primitives into one opaque extern call

Backends need to know too much about runtime helpers, host imports, and pure
ops for that to scale.

## 7. The Most Important IR Conclusion

For `raml`, wasm work is not asking for "a Wasm emitter".

It is asking for a compiler structure where:

- one shared `Raml Core IR` preserves the semantics that native, JS, and wasm
  all need
- backend-specific IRs own representation and host commitments
- artifacts and runtime sidecars are first-class products, not late hacks

That is the IR shape most consistent with the source evidence.
